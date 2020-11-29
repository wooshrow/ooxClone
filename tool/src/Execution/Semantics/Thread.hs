module Execution.Semantics.Thread where

import qualified Data.Stack as T
import qualified Data.Map as M
import           Control.Monad (foldM)
import           Control.Lens ((&), (^.), (%~), (?~))
import           Data.Configuration
import           Text.Pretty
import           Analysis.CFA.CFG
import           Execution.Semantics.StackFrame
import           Execution.Semantics.Exception
import           Execution.Semantics.Evaluation
import           Execution.State
import           Execution.State.Thread
import           Language.Syntax
     
--------------------------------------------------------------------------------
-- Stack Frame Management
--------------------------------------------------------------------------------

pushStackFrameOnCurrentThread :: ExecutionState -> Node -> DeclarationMember -> Maybe Lhs -> [(Parameter, Expression)] -> Engine r ExecutionState
pushStackFrameOnCurrentThread state0 returnPoint member lhs params
    | Just tid <- state0 ^. currentThreadId = 
        pushStackFrame state0 tid returnPoint member lhs params
    | otherwise =
        stop state0 "pushStackFrameOnCurrentThread: cannot get current thread"

pushStackFrame :: ExecutionState -> ThreadId -> Node -> DeclarationMember -> Maybe Lhs -> [(Parameter, Expression)] -> Engine r ExecutionState
pushStackFrame state0 tid returnPoint member lhs params = do
    let frame0 = StackFrame returnPoint lhs M.empty member
    state1 <- incrementLastHandlerPops state0
    case getThread state1 tid of
        Just thread0 -> do
            (state2, frame1) <- foldM (writeParam thread0) (state1, frame0) params
            let thread1 = thread0 & (callStack %~ T.push frame1)
            return $ updateThreadInState state2 thread1
        Nothing      ->
            stop state1 "pushStackFrame: cannot get current thread"

writeParam :: Thread -> (ExecutionState, StackFrame) -> (Parameter, Expression) -> Engine r (ExecutionState, StackFrame)
writeParam thread (stateN, frameN) (Parameter _ name _, value0)
    -- The initial call
    | T.null (thread ^. callStack) && processTid == (thread ^. parent) = do
        debug ("Writing parameter '" ++ toString name ++ "'")
        return (stateN, writeDeclarationOnFrame frameN name value0)
    -- A fork call
    | processTid /= (thread ^. parent) = do
        debug ("Writing and evaluating parameter '" ++ toString name ++ "'")
        (stateN', value1) <- evaluate (stateN & (currentThreadId ?~ (thread ^. parent))) value0
        return (stateN' & (currentThreadId ?~ (thread ^. tid)), writeDeclarationOnFrame frameN name value1)
    -- A regular call
    | otherwise = do
        debug ("Writing and evaluating parameter '" ++ toString name ++ "'")
        (stateN', value1) <- evaluate stateN value0
        return (stateN', writeDeclarationOnFrame frameN name value1)

popStackFrame :: ExecutionState -> Engine r ExecutionState
popStackFrame state0 = do
    state1 <- decrementLastHandlerPops state0
    case getCurrentThread state1 of
        Just thread0 -> do
            let thread1 = thread0 & (callStack %~ T.pop)
            return $ updateThreadInState state1 thread1
        Nothing     -> 
            stop state1 "popStackFrame: cannot get current thread"

isLastStackFrame :: ExecutionState -> Bool
isLastStackFrame state =
    let thread = getCurrentThread state
     in maybe True ((1 ==) . T.size . (^. callStack)) thread