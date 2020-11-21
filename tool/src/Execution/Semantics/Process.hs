module Execution.Semantics.Process(
      spawn
    , despawnCurrentThread
    , freshTid
) where

import qualified Data.Stack as T
import qualified Data.Set as S
import           Polysemy.Error
import           Analysis.CFA.CFG
import           Control.Lens hiding (assign)
import           Data.Configuration
import           Execution.Semantics.Thread
import           Execution.State
import           Execution.State.Thread
import           Language.Syntax
import qualified Language.Syntax.Lenses as SL
import           Verification.Result

spawn :: ExecutionState -> ThreadId -> Node -> DeclarationMember -> [Expression] -> Engine r (ExecutionState, ThreadId)
spawn state0 parent entry member arguments = do
    cfg <- askCFG
    let tid        = freshTid state0
    let pc         = context cfg entry
    let thread     = Thread tid parent pc T.empty T.empty
    let state1     = updateThreadInState state0 thread
    let parameters = member ^?! SL.params
    (,tid) <$> pushStackFrame state1 tid entry member Nothing (zip parameters arguments)

despawnCurrentThread :: ExecutionState -> Engine r ExecutionState
despawnCurrentThread state0
    | Just thread <- getCurrentThread state0 = do
        state1 <- despawn state0 thread
        return $ state1 & (currentThreadId .~ Nothing)
    | otherwise =
        throw (InternalError "despawnCurrentThread: cannot get current thread")

despawn :: ExecutionState -> Thread -> Engine r ExecutionState
despawn state thread = do
    debug ("Despawning thread with id '" ++ show (thread ^. tid) ++ "'")
    return $ state & (threads %~ S.delete thread)

freshTid :: ExecutionState -> ThreadId
freshTid state = ThreadId (1 + state ^. numberOfForks)