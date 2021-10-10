module Execution.Semantics.PartialOrderReduction(
      isEnabled
    , por
) where

import Debug.Trace
import qualified GHC.Stack as GHC
import qualified Data.Set as S
import           Control.Lens ((&), (^?!), (^.), (.~), (?~))
import           Control.Monad
import           Data.Configuration
import           Data.Statistics
import           Execution.Effects
import           Execution.Errors
import           Execution.State
import           Execution.State.Thread
import           Execution.State.LockSet as LockSet
import           Execution.State.AliasMap as AliasMap (lookup)
import           Execution.State.InterleavingConstraints
import           Execution.Semantics.StackFrame
import           Analysis.CFA.CFG
import           Language.Syntax
import           Language.Syntax.Fold
import           Language.Syntax.DSL
import qualified Language.Syntax.Lenses as SL
import           Text.Pretty

isEnabled :: GHC.HasCallStack => ExecutionState -> Thread -> Engine r Bool
isEnabled state thread
    | (_, _, StatNode (Lock var _ _), _) <- thread ^. pc = do
        ref <- readDeclaration (state & currentThreadId ?~ (thread ^. tid)) var
        case ref of
            Lit NullLit{} _ _ ->
                infeasible
            SymbolicRef{} ->
                return True
            Ref ref _ _ ->
                case LockSet.lookup ref (state ^. locks) of
                    Just tid' -> return (tid' == thread ^. tid)
                    Nothing   -> return True
            _ ->
                stop state (expectedReferenceErrorMessage ref)
    | (_, _, StatNode (Join _ _), _) <- thread ^. pc =
        S.null <$> children state (thread ^. tid)
    | otherwise =
        return True

children :: ExecutionState -> ThreadId -> Engine r (S.Set Thread)
children state tid =
    return $ S.filter (\ thread -> thread ^. parent == tid) (state ^. threads)

por :: GHC.HasCallStack => ExecutionState -> [Thread] -> Engine r (ExecutionState, [Thread])
por state0 []      = deadlock state0
por state0 ts@[singleThread] = return (state0,ts)
por state0 threads = do
    config <- askConfig
    if not (applyPOR config)
        then
            return (state0, threads)
        else do
            onlyDoLocals <-  filterM  (nextActionIsLocal state0) threads
            -- calculate the interleaving constraints on the current state:
            let remainingThreads = filter (\t-> not (t `elem` onlyDoLocals)) threads
            --state1_ <- generate state0 threads
            state1_ <- generate state0 remainingThreads
            let currentConstraints = state1_ ^. interleavingConstraints
            --let uniqueThreads = filter (not . isRedundant state0 currentConstraints) threads
            let uniqueThreads = filter (not . isRedundant state0 currentConstraints) remainingThreads
            let uniqueThreads2 = if null onlyDoLocals then uniqueThreads else take 1 onlyDoLocals
            -- let uniqueThreads2 = uniqueThreads
            -- measurePrunes (length threads - length uniqueThreads)
            measurePrunes (length threads - length uniqueThreads2)
            -- state1 <- generate state0 threads
            let state1__ = if null onlyDoLocals then state1_ else state0
            return (state1__ , uniqueThreads2)

{-
   Check if executing the given thread would be redundant (and hence
   we can afford not to execute it).
   Let tx be the thread under consideration, and let b be the next primitive
   action to execution in tx.
   Let s be the current state under consideration.

   There are two cases where tx is redundant.

   Case-1. We have a previous state, let's call it s0, and let IC0 be
   the set of all interleaving constraints in that previous state s0.

   If (1) there exists a constraint b~c in IC0 (b and some c are independent),
   and (2) b<c, and (3) c was executed last (so, in the transition from s0
   to the current state under consideration). This would imply that the sequence
   b;c was or will be tried on the state s0, which would lead to the same state
   as b on the current state s (or in other words, on c;b on s0). Since executing
   b (of tx) leads to a duplicated state, tx is thus redundant.

   Else, we have Case-2. Let IC be the set of all interleaving constraints on
   the current state s.

   If (1) there is NO constraints b!~c in IC (b and c are dependent), and
   (2) for all constraints a~b in IC we have a<b. Condition-1 implies that
   there is no currently enabled thread whose behavior depends specifically on
   what b does on s. Condition-2 implies that a;b will be tried on the state s,
   so b will get its turn. It will be tried on the state s--a-->s', but since
   b does not depend on what a does on s, we can try it on s' without changing
   its effect.
-}
isRedundant :: ExecutionState -> InterleavingConstraints -> Thread -> Bool
isRedundant state currentConstraints thread =
    -- case-1:
    if any (\case IndependentConstraint b c -> fst b == myTid && fst c `isLastStepOf` executiontrace)
         relevantIndependentPreviousConstraints
    then True
    else False {-
         WP note: ok, so this idea of redundant2 is unsound.
         if redundant2
         then trace ("\n>>> droping th-" ++ show (_tid thread)) redundant2
         -- then redundant2
         else redundant2 -}

    where
        redundant2 = null relevantDependentCurrentConstraints
                     && any (\case IndependentConstraint a b -> if fst b == myTid then trace ("\n=== indep " ++ show_ a ++ "\n   vs " ++ show_ b ++ "\n   same pair??" ++ show (fst a== fst b)) True else False)
                     -- && any (\case IndependentConstraint a b -> fst b == myTid )
                        relevantIndependentCurrentConstraints

        -- show_ v = show (pretty v)
        show_ cfgContext@(tid,(_,_,nodevalue,_)) = show tid ++ ":" ++ show (pretty nodevalue)

        myTid = _tid thread
        currentStmt = (thread ^. pc)
        prevConstraints = state ^. interleavingConstraints

        relevantIndependentPreviousConstraints = filter p prevConstraints
           where
           p (IndependentConstraint a b) = fst a == myTid || fst b == myTid
           p _ = False

        -- constraints that have anything to do with the current thread:
        relevantDependentCurrentConstraints = filter p currentConstraints
           where
           p (NotIndependentConstraint a b) = fst a == myTid || fst b == myTid
           p _ = False

        relevantIndependentCurrentConstraints = filter p currentConstraints
           where
           p (IndependentConstraint a b) = fst a == myTid || fst b == myTid
           p _ = False

        executiontrace   = map fst (state ^. programTrace)

        isLastStepOf a [] = False
        isLastStepOf a (b:_) = a==b

-- given the current state s, its set of interleaving-constraints consists
-- actuall of the constraints at the previous state s0 that leads to to
-- the current state s. The function below calculates the new set of
-- interleaving constraints on this s, and then updating this into s, to
-- prepare it for the transition to the next state.
generate :: GHC.HasCallStack => ExecutionState -> [Thread] -> Engine r ExecutionState
generate state threads = do
    -- generate the constraints for every pair of threads (x,y). Note that
    -- the pair is ordered x<y. :
    let pairs = [(x, y) | let list = threads, x <- list, y <- list, x < y]
    new <- foldM construct [] pairs
    -- WP update 2; we drop all existing constraints and just put the new ones:
    -- return $ updateInterleavingConstraints state new
    return (state & (interleavingConstraints .~ new))
    where
        -- constructing the interleaving constraint for a given pair of
        -- threads (t1,t2) ... the ordering is t1<t2.
        construct :: InterleavingConstraints -> (Thread, Thread) -> Engine r InterleavingConstraints
        construct acc pair@(thread1, thread2) = do
            isIndep <- isIndependent state pair
            let t1 = _tid thread1
            let t2 = _tid thread2
            if isIndep
                then return (IndependentConstraint (t1, thread1 ^. pc) (t2, thread2 ^. pc) : acc)
                else return (NotIndependentConstraint (t1, thread1 ^. pc) (t2, thread2 ^. pc) : acc)


updateInterleavingConstraints :: ExecutionState -> InterleavingConstraints -> ExecutionState
updateInterleavingConstraints state new = do
    let original = state ^. interleavingConstraints
    -- let filtered = filter (isConflict new) original
    -- renaming ... notConflicting is a better name :|
    let filtered = filter (notConflicting new) original
    -- in the update, we include all new constrainst + some of the old
    -- constraints, if they do not conflict with the new ones:
    state & (interleavingConstraints .~ (filtered ++ new))

{-
isConflict :: [InterleavingConstraint] -> InterleavingConstraint -> Bool
isConflict _   IndependentConstraint{}          = False
isConflict new (NotIndependentConstraint x1 y1) = flip any new $ \case
    (IndependentConstraint x2 y2) -> S.fromList [x1, y1] `S.disjoint` S.fromList [x2, y2]
    NotIndependentConstraint{}    -> False
-}

-- renaming:
-- Check if a given old constraint c would be non-conflicting with a set of
-- new constraints.
-- This seems to allow only non-independence constraints...
--
notConflicting :: [InterleavingConstraint] -> InterleavingConstraint -> Bool
-- if c specifies dependency is considered as conflicing... so we drop it:
notConflicting  _   IndependentConstraint{}          = False
-- if c specifies non-dependency between threads x1 and y1; we keep it (it is
-- non-conflicting) if either x1 or y1 appears in an Independence constraint in
-- the new. ??? that does not make sense....
notConflicting  new (NotIndependentConstraint x1 y1) = flip any new $ \case
    (IndependentConstraint x2 y2) -> S.fromList [x1, y1] `S.disjoint` S.fromList [x2, y2]
    NotIndependentConstraint{}    -> False


type ReadWriteSet = (S.Set Reference, S.Set Reference)

--
-- WP variant
-- check if a thread's first action only access local-vars:
nextActionIsLocal :: GHC.HasCallStack => ExecutionState -> Thread -> Engine r Bool
nextActionIsLocal state thread = do
    (writes,rds) <- dependentOperationsOfT state thread
    return (null writes && null rds)


isIndependent :: GHC.HasCallStack => ExecutionState -> (Thread, Thread) -> Engine r Bool
isIndependent state (thread1, thread2) = do
    (wT1, rT1) <- dependentOperationsOfT state thread1
    (wT2, rT2) <- dependentOperationsOfT state thread2
    -- WP variant, if thread1 only access local-vars and tr2 not, we will
    -- declare t1,t2 (in that direction!) to be dependent:
    return $ S.disjoint wT1 wT2 && S.disjoint rT1 wT2 && S.disjoint rT2 wT1
    {- if (null wT1 && null rT1) || (null wT2 && null rT2)
       then return False
       else if (S.member minBound wT1 && (not(null wT2) || not(null rT2)))
                || (S.member minBound rT1 && not(null wT2))
                || (S.member minBound wT2 && (not(null wT1) || not(null rT1)))
                || (S.member minBound rT2 && not(null wT1))
            then return False
            else return $ S.disjoint wT1 wT2 && S.disjoint rT1 wT2 && S.disjoint rT2 wT1 -}



-- | Returns the reads and writes of the current thread.
dependentOperationsOfT :: GHC.HasCallStack => ExecutionState -> Thread -> Engine r ReadWriteSet
dependentOperationsOfT state thread = dependentOperationsOfN state (thread ^. tid) (thread ^. pc)

-- | Returns the reads and writes of the current program counter.
dependentOperationsOfN :: GHC.HasCallStack => ExecutionState -> ThreadId -> CFGContext -> Engine r ReadWriteSet
dependentOperationsOfN state tid (_, _, StatNode stat, _)
    = dependentOperationsOfS state tid stat
dependentOperationsOfN _ _ _
    = return (S.empty, S.empty)

-- | Returns the reads and writes of the current statement.
dependentOperationsOfS :: GHC.HasCallStack => ExecutionState -> ThreadId -> Statement -> Engine r ReadWriteSet
dependentOperationsOfS state tid (Assign lhs rhs _ _) = (,)        <$> dependentOperationsOfLhs state tid lhs <*> dependentOperationsOfRhs state tid rhs
dependentOperationsOfS state tid (Assert ass _ _)     = (,S.empty) <$> dependentOperationsOfE state tid ass
dependentOperationsOfS state tid (Assume ass _ _)     = (,S.empty) <$> dependentOperationsOfE state tid ass
dependentOperationsOfS state tid (Lock var _ _)       = (\ refs -> (refs, refs)) <$> getReferences state tid var
dependentOperationsOfS state tid (Unlock var _ _)     = (\ refs -> (refs, refs)) <$> getReferences state tid var
dependentOperationsOfS _     _   _                    = return (S.empty, S.empty)

dependentOperationsOfLhs :: ExecutionState -> ThreadId -> Lhs -> Engine r (S.Set Reference)
dependentOperationsOfLhs _     _   LhsVar{}               = return S.empty
dependentOperationsOfLhs state tid (LhsField var _ _ _ _) = getReferences state tid var
dependentOperationsOfLhs state tid (LhsElem var _ _ _)    = getReferences state tid var

dependentOperationsOfRhs :: ExecutionState -> ThreadId -> Rhs -> Engine r (S.Set Reference)
dependentOperationsOfRhs _     _   RhsExpression{}      = return S.empty
dependentOperationsOfRhs state tid (RhsField var _ _ _) = getReferences state tid (var ^?! SL.var)
dependentOperationsOfRhs state tid (RhsElem var _ _ _)  = getReferences state tid (var ^?! SL.var)
dependentOperationsOfRhs _     _   RhsCall{}            = return S.empty
dependentOperationsOfRhs _     _   RhsArray{}           = return S.empty

dependentOperationsOfE :: GHC.HasCallStack => ExecutionState -> ThreadId -> Expression -> Engine r (S.Set Reference)
dependentOperationsOfE state tid = foldExpression algebra
    where
        algebra = monoidMExpressionAlgebra
            { fForall = \ _ _ domain _ _ _ -> getReferences state tid domain
            , fExists = \ _ _ domain _ _ _ -> getReferences state tid domain }

getReferences :: GHC.HasCallStack => ExecutionState -> ThreadId -> Identifier -> Engine r (S.Set Reference)
getReferences state tid var = do
    ref <- readDeclaration (state & currentThreadId ?~ tid) var
    case ref of
        Lit NullLit{} _ _ ->
            return S.empty
        Ref{} ->
            return $ S.singleton (ref ^?! SL.ref)
        SymbolicRef{}     ->
            case AliasMap.lookup (ref ^?! SL.var) (state ^. aliasMap) of
                Just aliases -> return . S.map (^?! SL.ref) . S.filter (/= lit' nullLit') $ aliases
                Nothing      -> return $ S.singleton minBound
                                -- stop state (trace (">>> " ++ show var) noAliasesErrorMessage)
        _ ->
            stop state (expectedReferenceErrorMessage ref)
