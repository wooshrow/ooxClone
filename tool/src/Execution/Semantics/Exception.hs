module Execution.Semantics.Exception where

import qualified Data.Stack as T
import           Analysis.CFA.CFG
import           Data.Configuration
import           Control.Lens ((&), (^.), (%~))
import           Execution.State
import           Execution.State.Thread

insertHandler :: ExecutionState -> Node -> Engine r ExecutionState
insertHandler state handler 
    | Just thread0 <- getCurrentThread state = do
        debug ("Inserting an exception handler '" ++ show handler ++ "'")
        let thread1 = thread0 & (handlerStack %~ T.push (handler, 0))
        return $ updateThreadInState state thread1
    | otherwise = 
        stop state "insertHandler: cannot get current thread"
    
findLastHandler :: ExecutionState -> Maybe (Node, Int)
findLastHandler state = do
    thread <- getCurrentThread state
    T.peek (thread ^. handlerStack)

removeLastHandler :: ExecutionState -> Engine r ExecutionState
removeLastHandler state
    | Just thread0 <- getCurrentThread state = do
        let thread1 = thread0 & (handlerStack %~ T.pop)
        return $ updateThreadInState state thread1
    | otherwise = 
        stop state "removeLastHandler: cannot get current thread"

incrementLastHandlerPops :: ExecutionState -> Engine r ExecutionState
incrementLastHandlerPops state
    | Just thread1         <- getCurrentThread state
    , Just (handler, pops) <- T.peek (thread1 ^. handlerStack) = do
        debug ("Incrementing exception handler stack pop to '" ++ show (pops + 1) ++ "'")
        let thread2 = thread1 & (handlerStack %~ T.updateTop (handler, pops + 1))
        return $ updateThreadInState state thread2
    | otherwise = 
        return state

decrementLastHandlerPops :: ExecutionState -> Engine r ExecutionState
decrementLastHandlerPops state
    | Nothing <- thread0 =
        stop state "decrementLastHandlerPops: cannot get current thread"
    | Just thread1         <- thread0
    , Just (handler, pops) <- T.peek (thread1 ^. handlerStack) = do
        debug ("Decrementing exception handler stack pop to '" ++ show (pops - 1) ++ "'")
        let thread2 = thread1 & (handlerStack %~ T.updateTop (handler, pops - 1))
        return $ updateThreadInState state thread2
    | otherwise =
        return state
    where thread0 = getCurrentThread state