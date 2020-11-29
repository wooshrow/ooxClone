module Execution.Semantics.Process(
      spawn
    , despawnCurrentThread
    , freshTid
) where

import qualified Data.Stack as T
import qualified Data.Set as S
import           Analysis.CFA.CFG
import           Control.Lens ((&), (^?!), (^.), (%~), (+~), (.~))
import           Execution.Semantics.Thread
import           Execution.Effects
import           Execution.State
import           Execution.State.Thread
import           Language.Syntax
import qualified Language.Syntax.Lenses as SL

spawn :: ExecutionState -> ThreadId -> Node -> DeclarationMember -> [Expression] -> Engine r (ExecutionState, ThreadId)
spawn state0 parent entry member arguments = do
    cfg <- askCFG
    let tid        = freshTid state0
    let pc         = context cfg entry
    let thread     = Thread tid parent pc T.empty T.empty
    let state1     = updateThreadInState state0 thread
    let parameters = member ^?! SL.params
    state2 <- pushStackFrame state1 tid entry member Nothing (zip parameters arguments)
    return (state2 & (numberOfForks +~ 1), tid)

despawnCurrentThread :: ExecutionState -> Engine r ExecutionState
despawnCurrentThread state0
    | Just thread <- getCurrentThread state0 = do
        state1 <- despawn state0 thread
        return $ state1 & (currentThreadId .~ Nothing)
    | otherwise =
        stop state0 "despawnCurrentThread: cannot get current thread"

despawn :: ExecutionState -> Thread -> Engine r ExecutionState
despawn state thread = do
    debug ("Despawning thread with id '" ++ show (thread ^. tid) ++ "'")
    return $ state & (threads %~ S.delete thread)

freshTid :: ExecutionState -> ThreadId
freshTid state = ThreadId (1 + state ^. numberOfForks)