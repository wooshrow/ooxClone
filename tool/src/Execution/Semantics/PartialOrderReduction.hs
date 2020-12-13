module Execution.Semantics.PartialOrderReduction(
      isEnabled
    , por
) where

import qualified Data.Set as S
import           Control.Lens ((&), (^?!), (^.), (.~), (?~))
import           Control.Monad
import           Text.Pretty
import           Data.Configuration
import           Execution.Effects
import           Execution.State
import           Execution.State.Thread
import           Execution.State.LockSet as LockSet
import           Execution.State.AliasMap as AliasMap
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
            Lit NullLit{} _ _ -> infeasible
            SymbolicRef{}     -> return True
            Ref ref _ _       -> 
                case LockSet.lookup ref (state ^. locks) of
                    Just tid' -> return (tid' == thread ^. tid)
                    Nothing   -> return True
            _                 -> 
                stop state ("isEnabled: non-reference '" ++ toString ref ++ "'")
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
            uniqueThreads <- filterM (isUniqueInterleaving state0) threads
            state1 <- generateConstraints state0 threads
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
            isIndep <- isIndependent state pair
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
dependentOperationsOfN state tid (_, _, CallNode _ _ Nothing arguments _, _) 
    = (,S.empty) . S.unions <$> mapM (dependentOperationsOfE state tid) arguments
--dependentOperationsOfN thread (_, _, CallNode _ _ (Just this) arguments _, _) 
--    = (,S.empty) . S.unions <$> mapM (dependentOperationsOfE thread) arguments
dependentOperationsOfN _ _ _                        
    = return (S.empty, S.empty)

-- | Returns the reads and writes of the current statement.
dependentOperationsOfS :: ExecutionState -> ThreadId -> Statement -> Engine r ReadWriteSet
dependentOperationsOfS state tid (Assign lhs rhs _ _) = (,)        <$> dependentOperationsOfLhs state tid lhs <*> dependentOperationsOfRhs state tid rhs
dependentOperationsOfS state tid (Assert ass _ _)     = (,S.empty) <$> dependentOperationsOfE state tid ass
dependentOperationsOfS state tid (Assume ass _ _)     = (,S.empty) <$> dependentOperationsOfE state tid ass
dependentOperationsOfS state tid (Return expr _ _)    = (,S.empty) <$> maybe (return S.empty) (dependentOperationsOfE state tid) expr
dependentOperationsOfS state tid (Lock var _ _)       = (\ refs -> (refs, refs)) <$> getReferences state tid var
dependentOperationsOfS state tid (Unlock var _ _)     = (\ refs -> (refs, refs)) <$> getReferences state tid var
dependentOperationsOfS state tid (Fork inv _ _)       = (,S.empty) <$> dependentOperationsOfI state tid inv
dependentOperationsOfS _     _   _                    = return (S.empty, S.empty)

dependentOperationsOfLhs :: ExecutionState -> ThreadId -> Lhs -> Engine r (S.Set Reference)
dependentOperationsOfLhs _     _   LhsVar{}               = return S.empty
dependentOperationsOfLhs state tid (LhsField var _ _ _ _) = getReferences state tid var
dependentOperationsOfLhs state tid (LhsElem var _ _ _)    = getReferences state tid var

dependentOperationsOfRhs :: ExecutionState -> ThreadId -> Rhs -> Engine r (S.Set Reference)
dependentOperationsOfRhs state tid (RhsExpression value _ _) = dependentOperationsOfE state tid value
dependentOperationsOfRhs state tid (RhsField var _ _ _)      = getReferences state tid (var ^?! SL.var)
dependentOperationsOfRhs state tid (RhsElem var _ _ _)       = getReferences state tid (var ^?! SL.var)
dependentOperationsOfRhs state tid (RhsCall inv _ _)         = dependentOperationsOfI state tid inv
dependentOperationsOfRhs state tid (RhsArray _ sizes _ _)    = S.unions <$> mapM (dependentOperationsOfE state tid) sizes

dependentOperationsOfI :: ExecutionState -> ThreadId -> Invocation -> Engine r (S.Set Reference)
dependentOperationsOfI state tid inv 
    = S.unions <$> mapM (dependentOperationsOfE state tid) (inv ^. SL.arguments)

dependentOperationsOfE :: ExecutionState -> ThreadId -> Expression -> Engine r (S.Set Reference)
dependentOperationsOfE state tid = foldExpression algebra
    where
        algebra = monoidMExpressionAlgebra
            { fForall = undefined
            , fExists = undefined
            , fSizeOf = \ var _ _ -> getReferences state tid var }

getReferences :: ExecutionState -> ThreadId -> Identifier -> Engine r (S.Set Reference)
getReferences state tid var = do
    ref <- readDeclaration (state & currentThreadId ?~ tid) var
    case ref of
        Lit NullLit{} _ _ -> return S.empty
        Ref{}             -> return $ S.singleton (ref ^?! SL.ref)
        SymbolicRef{}     ->
            case AliasMap.lookup (ref ^?! SL.var) (state ^. aliasMap) of
                Just aliases -> return . S.map (^?! SL.ref) . S.filter (/= lit' nullLit') $ aliases
                Nothing      -> stop state "getReferences: no aliases"
        _                 ->
            stop state "getReferences: non-reference"