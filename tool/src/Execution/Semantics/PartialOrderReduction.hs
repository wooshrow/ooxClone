module Execution.Semantics.PartialOrderReduction(
      isEnabled
    , por
) where

import qualified Data.Set as S
import           Control.Lens hiding (children)
import           Polysemy.Error
import           Text.Pretty
import           Execution.State
import           Execution.State.Thread
import           Execution.State.LockSet as LockSet
import           Execution.State.InterleavingConstraints
import           Execution.Semantics.StackFrame
import           Analysis.CFA.CFG
import           Language.Syntax
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
por state0 threads = return (state0, threads)

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