module Execution.Semantics.Exception where

import qualified Data.Stack as T
import           Analysis.CFA.CFG
import           Polysemy.Error
import           Control.Lens hiding (assign)
import           Execution.State
import           Execution.State.Thread
import           Verification.Result

insertHandler :: ExecutionState -> Node -> Engine r ExecutionState
insertHandler state handler 
    | Nothing <- thread0 = 
        throw (InternalError "insertHandler: cannot get current thread")
    | Just thread1 <- thread0 = do
        let thread2 = thread1 & (handlerStack %~ T.push (handler, 0))
        return $ updateThreadInState state thread2
    where thread0 = getCurrentThread state
    
findLastHandler :: ExecutionState -> Maybe (Node, Int)
findLastHandler state = do
    thread <- getCurrentThread state
    T.peek (thread ^. handlerStack)

removeLastHandler :: ExecutionState -> Engine r ExecutionState
removeLastHandler state
    | Nothing <- thread0 = 
        throw (InternalError "removeLastHandler: cannot get current thread")
    | Just thread1 <- thread0 = do
        let thread2 = thread1 & (handlerStack %~ T.pop)
        return $ updateThreadInState state thread2
    where thread0 = getCurrentThread state

incrementLastHandlerPops :: ExecutionState -> Engine r ExecutionState
incrementLastHandlerPops state
    | Just thread0         <- getCurrentThread state
    , Just (handler, pops) <- T.peek (thread0 ^. handlerStack) = do
        let thread1 = thread0 & (handlerStack %~ T.updateTop (handler, pops + 1))
        return $ updateThreadInState state thread1
    | otherwise = 
        return state

decrementLastHandlerPops :: ExecutionState -> Engine r ExecutionState
decrementLastHandlerPops state
    | Nothing <- thread0 =
        throw (InternalError "decrementLastHandlerPops: cannot get current thread")
    | Just thread1         <- thread0
    , Just (handler, pops) <- T.peek (thread1 ^. handlerStack) = do
        let thread2 = thread1 & (handlerStack %~ T.updateTop (handler, pops - 1))
        return $ updateThreadInState state thread2
    | otherwise =
        return state
    where thread0 = getCurrentThread state