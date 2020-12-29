module Execution.Semantics.Process(
      spawn
    , despawnCurrentThread
    , freshTid
) where

import qualified Data.Stack as T
import qualified Data.Set as S
import           Text.Pretty
import           Analysis.CFA.CFG
import           Control.Lens ((&), (^?!), (^.), (%~), (+~), (.~), Field1(_1))
import           Execution.Semantics.Thread
import           Execution.Effects
import           Execution.Errors
import           Execution.State
import           Execution.State.Thread
import           Language.Syntax
import qualified Language.Syntax.Lenses as SL

spawn :: ExecutionState -> ThreadId -> DeclarationMember -> [Expression] -> Engine r (ExecutionState, ThreadId)
spawn state0 parent member arguments = do
    cfg <- askCFG
    let tid        = freshTid state0
    let pc         = context cfg (member ^?! SL.labels ^. _1)
    let thread     = Thread tid parent pc T.empty T.empty
    let state1     = updateThreadInState state0 thread
    let parameters = member ^?! SL.params
    state2 <- pushStackFrame state1 tid (-1) member Nothing (zip parameters arguments)
    debug ("Spawning thread with thread id '" ++ toDebugString tid ++ "'")
    return (state2 & (numberOfForks +~ 1), tid)

despawnCurrentThread :: ExecutionState -> Engine r ExecutionState
despawnCurrentThread state0
    | Just thread <- getCurrentThread state0 = do
        state1 <- despawn state0 thread
        return $ state1 & (currentThreadId .~ Nothing)
    | otherwise =
        stop state0 (cannotGetCurrentThreadErrorMessage "despawnCurrentThread")

despawn :: ExecutionState -> Thread -> Engine r ExecutionState
despawn state thread = do
    debug ("Despawning thread with id '" ++ toDebugString (thread ^. tid) ++ "'")
    let index = S.findIndex thread (state ^. threads)
    return $ state & (threads %~ S.deleteAt index)

freshTid :: ExecutionState -> ThreadId
freshTid state = ThreadId (1 + state ^. numberOfForks)