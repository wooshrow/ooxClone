module Execution.Semantics.PartialOrderReduction(
      isEnabled
    , por
) where

import qualified Data.Set as S
import           Control.Lens hiding (children)
import           Control.Monad
import           Polysemy.Error
import           Text.Pretty
import           Data.Configuration
import           Execution.State
import           Execution.State.Thread
import           Execution.State.LockSet as LockSet
import           Execution.State.InterleavingConstraints
import           Execution.Semantics.StackFrame
import           Analysis.CFA.CFG
import           Language.Syntax
import           Language.Syntax.Fold
import qualified Language.Syntax.Lenses as SL
import           Verification.Result

isEnabled :: ExecutionState -> Thread -> Engine r Bool
isEnabled state thread
    | (_, _, StatNode (Lock var _ _), _) <- thread ^. pc = do
        ref <- readDeclaration (state & currentThreadId ?~ (thread ^. tid)) var
        case ref of
            Lit NullLit{} _ _ -> infeasible
            SymbolicRef{}     -> return True
            Ref ref _ _       -> 
                case LockSet.lookup ref (state ^. locks) of
                    Just tid' -> return (tid' == thread ^. tid)
                    Nothing   -> return True
            _                 -> 
                throw (InternalError ("isEnabled: non-reference '" ++ toString ref ++ "'"))
    | (_, _, StatNode (Join _ _), _) <- thread ^. pc = 
        S.null <$> children state (thread ^. tid)
    | otherwise = 
        return True

children :: ExecutionState -> ThreadId -> Engine r (S.Set Thread)
children state tid =
    return $ S.filter (\ thread -> thread ^. parent == tid) (state ^. threads)

por :: ExecutionState -> [Thread] -> Engine r (ExecutionState, [Thread])
por state0 []      = return (state0, [])
por state0 threads = do
    config <- askConfig
    if not (applyPOR config)
        then
            return (state0, threads)
        else do
            uniqueThreads <- filterM (isUniqueInterleaving state0) threads
            state1 <- generateConstraints state0 threads
            {-newConstraints <- por enabledThreads
            updateInterleavingConstraints newConstraints-}
            return (state1, uniqueThreads)

isUniqueInterleaving :: ExecutionState -> Thread -> Engine r Bool
isUniqueInterleaving state thread = do
    let trace       = state ^. programTrace
    let constraints = state ^. interleavingConstraints
    return . not . any (isUnique trace) $ constraints
    where
        isUnique trace (IndependentConstraint previous current)
            = (thread ^. pc) == current && previous `elem` trace
        isUnique _ NotIndependentConstraint{}
            = False

generateConstraints :: ExecutionState -> [Thread] -> Engine r ExecutionState
generateConstraints state threads = do
    let pairs = [(x, y) | let list = threads, x <- list, y <- list, x < y]
    newConstraints <- foldM construct [] pairs
    updateInterleavingConstraints state newConstraints
    where 
        construct :: InterleavingConstraints -> (Thread, Thread) -> Engine r InterleavingConstraints
        construct acc pair@(thread1, thread2) = do
            isIndep <- isIndependent pair
            if isIndep 
                then return (IndependentConstraint (thread1 ^. pc) (thread2 ^. pc) : acc)
                else return (NotIndependentConstraint (thread1 ^. pc) (thread2 ^. pc) : acc)

updateInterleavingConstraints :: ExecutionState -> InterleavingConstraints -> Engine r ExecutionState
updateInterleavingConstraints state newConstraints = do
    let constraints    = state ^. interleavingConstraints
    let oldConstraints = Prelude.filter (isConflict newConstraints) constraints
    return $ state & (interleavingConstraints .~ (oldConstraints ++ newConstraints))

isConflict :: [InterleavingConstraint] -> InterleavingConstraint -> Bool
isConflict _              (IndependentConstraint _ _)      = False
isConflict newConstraints (NotIndependentConstraint x1 y1) =
    any (\case (IndependentConstraint x2 y2)  -> S.fromList [x1, y1] `S.disjoint` S.fromList [x2, y2]
               (NotIndependentConstraint _ _) -> False) newConstraints

{-thread
    = case thread ^. pc of
        (_, _, StatNode (Lock var _ _), _) -> do 
            ref <- readVar thread var
            processRef ref
                (fmap not . locked (thread ^. tid))
                (const (return True))
                infeasible
        (_, _, StatNode (Join _ _), _) -> do
            children <- childrenOf thread
            return $ S.null children
        _   -> return True
-}

type ReadWriteSet = (S.Set Reference, S.Set Reference)

isIndependent :: (Thread, Thread) -> Engine r Bool
isIndependent (thread1, thread2) = do
    (wT1, rT1) <- dependentOperationsOfT thread1
    (wT2, rT2) <- dependentOperationsOfT thread2
    return $ S.disjoint wT1 wT2 && S.disjoint rT1 wT2 && S.disjoint rT2 wT1

-- | Returns the reads and writes of the current thread.
dependentOperationsOfT :: Thread -> Engine r ReadWriteSet
dependentOperationsOfT thread = dependentOperationsOfN thread (thread ^. pc)

-- | Returns the reads and writes of the current program counter.
dependentOperationsOfN :: Thread -> CFGContext -> Engine r ReadWriteSet
dependentOperationsOfN thread (_, _, StatNode stat, _) 
    = dependentOperationsOfS thread stat
dependentOperationsOfN thread (_, _, CallNode _ _ Nothing arguments _, _) 
    = (,S.empty) . S.unions <$> mapM (dependentOperationsOfE thread) arguments
--dependentOperationsOfN thread (_, _, CallNode _ _ (Just this) arguments _, _) 
--    = (,S.empty) . S.unions <$> mapM (dependentOperationsOfE thread) arguments
dependentOperationsOfN thread (_, _, ForkNode _ _ arguments, _) 
    = (,S.empty) . S.unions <$> mapM (dependentOperationsOfE thread) arguments
dependentOperationsOfN _ _                        
    = return (S.empty, S.empty)

-- | Returns the reads and writes of the current statement.
dependentOperationsOfS :: Thread -> Statement -> Engine r ReadWriteSet
dependentOperationsOfS thread (Assign lhs rhs _ _) = (\ l r -> (l,r)) <$> dependentOperationsOfLhs thread lhs <*> dependentOperationsOfRhs thread rhs
dependentOperationsOfS thread (Assert ass _ _)     = (,S.empty) <$> dependentOperationsOfE thread ass
dependentOperationsOfS thread (Assume ass _ _)     = (,S.empty) <$> dependentOperationsOfE thread ass
dependentOperationsOfS thread (Return expr _ _)    = (,S.empty) <$> maybe (return S.empty) (dependentOperationsOfE thread) expr
dependentOperationsOfS thread (Lock var _ _)       = (\ refs -> (refs, refs)) <$> getReferences thread var
dependentOperationsOfS thread (Unlock var _ _)     = (\ refs -> (refs, refs)) <$> getReferences thread var
dependentOperationsOfS _      _                    = return (S.empty, S.empty)

dependentOperationsOfLhs :: Thread -> Lhs -> Engine r (S.Set Reference)
dependentOperationsOfLhs _      LhsVar{}               = return S.empty
dependentOperationsOfLhs thread (LhsField var _ _ _ _) = getReferences thread var
dependentOperationsOfLhs thread (LhsElem var _ _ _)    = getReferences thread var

dependentOperationsOfRhs :: Thread -> Rhs -> Engine r (S.Set Reference)
dependentOperationsOfRhs thread (RhsExpression value _ _) = dependentOperationsOfE thread value
dependentOperationsOfRhs thread (RhsField var _ _ _)      = getReferences thread (var ^?! SL.var)
dependentOperationsOfRhs thread (RhsElem var _ _ _)       = getReferences thread (var ^?! SL.var)
dependentOperationsOfRhs thread (RhsCall inv _ _)         = dependentOperationsOfI thread inv
dependentOperationsOfRhs thread (RhsArray _ sizes _ _)    = S.unions <$> mapM (dependentOperationsOfE thread) sizes

dependentOperationsOfI :: Thread -> Invocation -> Engine r (S.Set Reference)
dependentOperationsOfI thread inv 
    = S.unions <$> mapM (dependentOperationsOfE thread) (inv ^. SL.arguments)

dependentOperationsOfE :: Thread -> Expression -> Engine r (S.Set Reference)
dependentOperationsOfE thread = foldExpression algebra
    where
        algebra = monoidMExpressionAlgebra -- TODO: quantifiers (- and var and ref)
            { fSizeOf = \ var _ _ -> getReferences thread var }

getReferences :: Thread -> Identifier -> Engine r (S.Set Reference)
getReferences thread var = undefined {-do
    ref <- readVar thread var
    processRef ref
        (return . S.singleton)
        (\ (SymbolicRef symRef _ _) -> do
            aliases <- fromJust <$> getAliasesWithoutNull symRef
            return $ S.map (^?! SL.ref) aliases)
        (return S.empty)-}