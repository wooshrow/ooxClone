module Execution.Semantics.PartialOrderReduction(
      isEnabled
    , por
) where

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

isEnabled :: ExecutionState -> Thread -> Engine r Bool
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
                stop state (expectedReferenceErrorMessage "isEnabled" ref)
    | (_, _, StatNode (Join _ _), _) <- thread ^. pc = 
        S.null <$> children state (thread ^. tid)
    | otherwise = 
        return True

children :: ExecutionState -> ThreadId -> Engine r (S.Set Thread)
children state tid =
    return $ S.filter (\ thread -> thread ^. parent == tid) (state ^. threads)

por :: ExecutionState -> [Thread] -> Engine r (ExecutionState, [Thread])
por state0 []      = deadlock state0
por state0 threads = do
    config <- askConfig
    if not (applyPOR config)
        then
            return (state0, threads)
        else do
            let uniqueThreads = filter (isUniqueInterleaving state0) threads
            measurePrunes (length threads - length uniqueThreads)
            state1 <- generate state0 threads
            return (state1, uniqueThreads)

isUniqueInterleaving :: ExecutionState -> Thread -> Bool
isUniqueInterleaving state thread = do
    let trace       = state ^. programTrace
    let constraints = state ^. interleavingConstraints
    not . any (isUnique trace) $ constraints
    where
        isUnique trace (IndependentConstraint previous current) = 
            (thread ^. pc) == current && previous `elem` trace
        isUnique _ NotIndependentConstraint{} = 
            False

generate :: ExecutionState -> [Thread] -> Engine r ExecutionState
generate state threads = do
    let pairs = [(x, y) | let list = threads, x <- list, y <- list, x < y]
    new <- foldM construct [] pairs
    return $ updateInterleavingConstraints state new
    where 
        construct :: InterleavingConstraints -> (Thread, Thread) -> Engine r InterleavingConstraints
        construct acc pair@(thread1, thread2) = do
            isIndep <- isIndependent state pair
            if isIndep 
                then return (IndependentConstraint (thread1 ^. pc) (thread2 ^. pc) : acc)
                else return (NotIndependentConstraint (thread1 ^. pc) (thread2 ^. pc) : acc)

updateInterleavingConstraints :: ExecutionState -> InterleavingConstraints -> ExecutionState
updateInterleavingConstraints state new = do
    let original = state ^. interleavingConstraints
    let filtered = filter (isConflict new) original
    state & (interleavingConstraints .~ (filtered ++ new))

isConflict :: [InterleavingConstraint] -> InterleavingConstraint -> Bool
isConflict _   IndependentConstraint{}          = False
isConflict new (NotIndependentConstraint x1 y1) = flip any new $ \case
    (IndependentConstraint x2 y2) -> S.fromList [x1, y1] `S.disjoint` S.fromList [x2, y2]
    NotIndependentConstraint{}    -> False

type ReadWriteSet = (S.Set Reference, S.Set Reference)

isIndependent :: ExecutionState -> (Thread, Thread) -> Engine r Bool
isIndependent state (thread1, thread2) = do
    (wT1, rT1) <- dependentOperationsOfT state thread1
    (wT2, rT2) <- dependentOperationsOfT state thread2
    return $ S.disjoint wT1 wT2 && S.disjoint rT1 wT2 && S.disjoint rT2 wT1

-- | Returns the reads and writes of the current thread.
dependentOperationsOfT :: ExecutionState -> Thread -> Engine r ReadWriteSet
dependentOperationsOfT state thread = dependentOperationsOfN state (thread ^. tid) (thread ^. pc)

-- | Returns the reads and writes of the current program counter.
dependentOperationsOfN :: ExecutionState -> ThreadId -> CFGContext -> Engine r ReadWriteSet
dependentOperationsOfN state tid (_, _, StatNode stat, _) 
    = dependentOperationsOfS state tid stat
dependentOperationsOfN _ _ _
    = return (S.empty, S.empty)

-- | Returns the reads and writes of the current statement.
dependentOperationsOfS :: ExecutionState -> ThreadId -> Statement -> Engine r ReadWriteSet
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

dependentOperationsOfE :: ExecutionState -> ThreadId -> Expression -> Engine r (S.Set Reference)
dependentOperationsOfE state tid = foldExpression algebra
    where
        algebra = monoidMExpressionAlgebra
            { fForall = \ _ _ domain _ _ _ -> getReferences state tid domain
            , fExists = \ _ _ domain _ _ _ -> getReferences state tid domain }

getReferences :: ExecutionState -> ThreadId -> Identifier -> Engine r (S.Set Reference)
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
                Nothing      -> stop state (noAliasesErrorMessage "getReferences")
        _ ->
            stop state (expectedReferenceErrorMessage "getReferences" ref)