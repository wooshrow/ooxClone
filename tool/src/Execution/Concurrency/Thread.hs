module Execution.Concurrency.Thread where

import qualified Data.Stack                 as T
import qualified Data.Map                   as M
import qualified Data.Set                   as S   
import           Polysemy.LocalState
import           Control.Lens hiding (index, indices, element, children)
import           Control.Monad
import           Data.Graph.Inductive.Graph (Node, context)
import           Text.Pretty
import           Data.Configuration
import           Execution.State
import           Execution.Memory.Heap
import {-# SOURCE #-} Execution.Evaluation
import           Execution.State.LockSet as LockSet
import           Execution.State.Thread
import           Analysis.CFA.CFG
import           Language.Syntax
import qualified Language.Syntax.Lenses as SL
import           Language.Syntax.Pretty()

readVar :: Thread -> Identifier -> Engine r Expression
readVar thread var
    | Just value <- (getLastStackFrame thread ^. declarations) M.!? var
        = case value of
            SymbolicRef{} -> initializeSymbolicRef value >> return value
            _             -> return value
    | otherwise 
        = error $ "failed to read '" ++ toString var ++ "'"

writeAndEvaluateVar :: Thread -> Identifier -> Expression -> Engine r Thread
writeAndEvaluateVar thread var value
    = writeVar thread var =<< evaluate thread value

writeVar :: Thread -> Identifier -> Expression -> Engine r Thread
writeVar thread0 var value = do
    let oldFrame = getLastStackFrame thread0
    let newFrame = oldFrame & (declarations %~ M.insert var value)
    let thread1  = thread0 & (callStack %~ T.updateTop newFrame)
    modifyLocal (\ state -> state & (threads %~ S.insert thread1))
    return thread1

--------------------------------------------------------------------------------
-- Stack Frame Management
--------------------------------------------------------------------------------

pushInitialStackFrame :: Thread -> DeclarationMember -> [(Parameter, Expression)] -> Engine r Thread
pushInitialStackFrame thread0 member params = do
    let thread1 = thread0 & (callStack %~ T.push (StackFrame (error "no return point in last stack frame") Nothing M.empty member))
    modifyLocal (\ state -> state & (threads %~ S.insert thread1))
    foldM writeParam thread1 params
    where writeParam threadN (Parameter _ name _, value) 
            = writeVar threadN name value

pushStackFrame :: Thread -> Thread -> Node -> DeclarationMember -> Maybe Lhs -> [(Parameter, Expression)] -> Engine r Thread
pushStackFrame thread0 evaluationThread returnPoint member lhs params = do
    let thread1 = thread0 & (callStack %~ T.push (StackFrame returnPoint lhs M.empty member))
    thread2 <- incrementLastHandlerPops thread1
    foldM writeParam thread2 params
    where
        writeParam threadN (Parameter _ name _, value)
            = writeVar threadN name =<< evaluate evaluationThread value

popStackFrame :: Thread -> Engine r Thread
popStackFrame thread0 = do
    thread1 <- decrementLastHandlerPops (thread0 & (callStack %~ T.pop))
    modifyLocal (\ state -> state & (threads %~ S.insert thread1))
    return thread1

isLastStackFrame :: Thread -> Bool
isLastStackFrame thread = T.size (thread ^. callStack) == 1

getLastStackFrame :: Thread -> StackFrame
getLastStackFrame thread = T.peekUnsafe (thread ^. callStack)

insertHandler :: Thread -> Node -> Engine r Thread
insertHandler thread0 handler = do
    let thread1 = thread0 & (handlerStack %~ T.push (handler, 0))
    modifyLocal (\ state -> state & (threads %~ S.insert thread1))
    return thread1
    
findLastHandler :: Thread -> Maybe (Node, Int)
findLastHandler thread = T.peek (thread ^. handlerStack)

removeLastHandler :: Thread -> Engine r Thread
removeLastHandler thread0 = do
    let thread1 = thread0 & (handlerStack %~ T.pop)
    modifyLocal (\ state -> state & (threads %~ S.insert thread1))
    return thread1

incrementLastHandlerPops :: Thread -> Engine r Thread
incrementLastHandlerPops thread0
    | Just (handler, pops) <- T.peek (thread0 ^. handlerStack) = do
        let thread1 = thread0 & (handlerStack %~ T.updateTop (handler, pops + 1))
        modifyLocal (\ state -> state & (threads %~ S.insert thread1))
        return thread1
    | otherwise
        = return thread0

decrementLastHandlerPops :: Thread -> Engine r Thread
decrementLastHandlerPops thread0
    | Just (handler, pops) <- T.peek (thread0 ^. handlerStack) = do
        let thread1 = thread0 & (handlerStack %~ T.updateTop (handler, pops - 1))
        modifyLocal (\ state -> state & (threads %~ S.insert thread1))
        return thread1
    | otherwise
        = return thread0
        
--------------------------------------------------------------------------------
-- Thread Spawning and Despawning
--------------------------------------------------------------------------------

spawnInitialThread :: Node -> DeclarationMember -> [Expression] -> Engine r Thread 
spawnInitialThread entry member arguments = do
    cfg <- askCFG
    tid <- freshTid
    let pc         = context cfg entry
    let parameters = member ^?! SL.params -- TODO: does not work for constructors or non-static
    debug "Spawning initial thread"
    newThread <- pushInitialStackFrame (Thread tid (ThreadId (-1)) pc T.empty T.empty) member (zip parameters arguments)
    modifyLocal (\ state -> state & (threads %~ S.insert newThread) & (numberOfForks +~ 1))
    return newThread

spawn :: Thread -> Node -> DeclarationMember -> [Expression] -> Engine r Thread
spawn parent entry member arguments = do
    cfg <- askCFG
    childTid <- freshTid
    let parentTid  = parent ^. tid
    let pc         = context cfg entry
    let parameters = member ^?! SL.params -- TODO: does not work for constructors or non-static
    debug ("Spawning new thread with parent id " ++ show parentTid)
    newThread <- pushStackFrame (Thread childTid parentTid pc T.empty T.empty) parent (error "undefined return point") member Nothing (zip parameters arguments)
    modifyLocal (\ state -> state & (threads %~ S.insert newThread) & (numberOfForks +~ 1))
    return newThread

despawn :: Thread -> Engine r ()
despawn thread = do
    debug ("Despawning thread with id " ++ show (thread ^. tid))
    modifyLocal (\ state -> state & (threads %~ S.delete thread))

isEnabled :: Thread -> Engine r Bool
isEnabled thread
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

freshTid :: Engine r ThreadId
freshTid = ThreadId . (1+) <$> getNumberOfForks

locked :: ThreadId -> Reference -> Engine r Bool
locked tid ref = do
    lockSet <- getLocks
    case LockSet.lookup ref lockSet of
        Nothing   -> return False
        Just lTid -> return (tid /= lTid)
        
lock :: ThreadId -> Reference -> Engine r ()
lock tid ref = modifyLocal (\ state -> state & locks %~ LockSet.insert ref tid)

unlock :: Reference -> Engine r ()
unlock ref = modifyLocal (\ state -> state & locks %~ LockSet.remove ref)

childrenOf :: Thread -> Engine r (S.Set Thread)
childrenOf thread
    = S.filter (\ thread' -> thread' ^. parent == thread ^. tid) <$> getThreads