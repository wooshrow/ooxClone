module Execution.Semantics.Thread where

import qualified Data.Stack as T
import qualified Data.Map as M
import           Control.Monad
import           Polysemy.Error
import           Control.Lens hiding (assign)
import           Analysis.CFA.CFG
import           Execution.Semantics.StackFrame
import           Execution.Semantics.Exception
import           Execution.Semantics.Evaluation
import           Execution.State
import           Execution.State.Thread
import           Language.Syntax
import           Verification.Result
     
--------------------------------------------------------------------------------
-- Stack Frame Management
--------------------------------------------------------------------------------

pushStackFrameOnCurrentThread :: ExecutionState -> Node -> DeclarationMember -> Maybe Lhs -> [(Parameter, Expression)] -> Engine r ExecutionState
pushStackFrameOnCurrentThread state0 returnPoint member lhs params
    | Just tid <- state0 ^. currentThreadId = 
        pushStackFrame state0 tid returnPoint member lhs params
    | otherwise =
        throw (InternalError "pushStackFrameOnCurrentThread: cannot get current thread")

pushStackFrame :: ExecutionState -> ThreadId -> Node -> DeclarationMember -> Maybe Lhs -> [(Parameter, Expression)] -> Engine r ExecutionState
pushStackFrame state0 tid returnPoint member lhs params
    | Nothing <- thread0 =
        throw (InternalError "pushStackFrame: cannot get current thread")
    | Just thread1 <- thread0 = do
        let frame0 = StackFrame returnPoint lhs M.empty member
        state1           <- incrementLastHandlerPops state0
        (state2, frame1) <- foldM writeParam (state1, frame0) params
        let thread2 = thread1 & (callStack %~ T.push frame1)
        return $ updateThreadInState state2 thread2
    where
        thread0 = getThread state0 tid
        writeParam (stateN, frameN) (Parameter _ name _, value0)
            | Just thread1 <- thread0, T.null (thread1 ^. callStack) = 
                return (stateN, writeDeclarationOnFrame frameN name value0)
            | otherwise = do
                (stateN', value1) <- evaluate stateN value0
                return (stateN', writeDeclarationOnFrame frameN name value1)

popStackFrame :: ExecutionState -> Engine r ExecutionState
popStackFrame state0 
    | Just thread0 <- getCurrentThread state0 = do
        state1 <- decrementLastHandlerPops state0
        let thread1 = thread0 & (callStack %~ T.pop)
        return $ updateThreadInState state1 thread1
    | otherwise =
        throw (InternalError "popStackFrame: cannot get current thread")

isLastStackFrame :: ExecutionState -> Bool
isLastStackFrame state =
    let thread = getCurrentThread state
     in maybe True ((1 ==) . T.size . (^. callStack)) thread