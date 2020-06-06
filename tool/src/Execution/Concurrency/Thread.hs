module Execution.Concurrency.Thread where

import qualified Data.Stack                 as T
import qualified Data.Map                   as M
import qualified Data.Set                   as S
import           Polysemy
import           Polysemy.State
import           Polysemy.Reader
import           Polysemy.Error
import           Polysemy.Cache
import           Control.Lens hiding (index, indices, element, children)
import           Control.Monad
import           Data.Graph.Inductive.Graph (Node, context)
import           Text.Pretty
import           Data.Positioned
import           Data.Configuration
import           Data.Statistics
import           Execution.ExecutionState
import           Execution.Memory.Heap
import           Execution.Concurrency.Lock
import           Analysis.CFA.CFG
import           Analysis.SymbolTable
import           Verification.Result
import           Language.Syntax
import           Language.Syntax.Fold
import           Language.Syntax.DSL
import qualified Language.Syntax.Lenses as SL
import           Language.Syntax.Pretty()

data Thread = Thread 
    { _tid          :: Int
    , _parent       :: Int
    , _pc           :: CFGContext
    , _callStack    :: T.Stack StackFrame
    , _handlerStack :: T.Stack (Node, Int) }

data StackFrame = StackFrame
    { _returnPoint   :: Node
    , _lhs           :: Maybe Lhs
    , _memory        :: M.Map Identifier Expression
    , _currentMember :: DeclarationMember }
    
$(makeLenses ''StackFrame)
$(makeLenses ''Thread)

instance Eq Thread where
    t1 == t2 = t1 ^. tid == t2 ^. tid  

instance Ord Thread where
    t1 <= t2 = t1 ^. tid <= t2 ^. tid
    
readVar :: Thread -> Identifier -> Engine r Expression
readVar thread var
    | Just value <- (getLastStackFrame thread ^. memory) M.!? var
        = case value of
            SymbolicRef{} -> initializeSymbolicRef value >> return value
            _             -> return value
    | otherwise 
        = error $ "failed to read '" ++ toString var ++ "'"

writeAndSubstituteVar :: Thread -> Identifier -> Expression -> Engine r Thread
writeAndSubstituteVar thread var value
    = writeVar thread var =<< substitute thread value

writeVar :: Thread -> Identifier -> Expression -> Engine r Thread
writeVar thread0 var value = do
    let oldFrame = getLastStackFrame thread0
    let newFrame = oldFrame & (memory %~ M.insert var value)
    let thread1  = thread0 & (callStack %~ T.updateTop newFrame)
    modifyLocal (\ state -> state & (threads %~ S.insert thread1))
    return thread1

substitute :: Thread -> Expression -> Engine r Expression
substitute thread0 expression = foldExpression substitutionAlgebra expression thread0
    where
        substitutionAlgebra :: Members [ Reader (Configuration, ControlFlowGraph, SymbolTable)
            , Error VerificationResult
            , Cache Expression
            , LocalState ExecutionState
            , State Statistics
            , Embed IO] r => ExpressionAlgebra (Thread -> Sem r Expression)
        substitutionAlgebra = ExpressionAlgebra
            { fForall = substituteQuantifier ands'

            , fExists = substituteQuantifier ors'
            
            , fBinOp = \ binOp lhs0 rhs0 ty pos thread -> do
                lhs1 <- lhs0 thread
                rhs1 <- rhs0 thread
                return (BinOp binOp lhs1 rhs1 ty pos)
                
            , fUnOp = \ unOp expr0 ty pos thread -> do
                expr1 <- expr0 thread
                return (UnOp unOp expr1 ty pos)
                
            , fVar = \ var _ _ thread -> do
                value <- readVar thread var
                case value of
                    SymbolicRef{} -> initializeSymbolicRef value
                    _             -> return ()
                return value
                
            , fSymVar = \ var ty pos _ ->
                return (SymbolicVar var ty pos)
                
            , fLit = \ lit ty pos _ -> 
                return (Lit lit ty pos)
                
            , fSizeOf = \ var _ _ thread -> do
                ref <- readVar thread var
                processRef ref
                    (fmap (lit' . intLit') . arraySize)
                    (error "substitute: SizeOf of Symbolic Reference")
                    infeasible
                    
            , fRef = \ ref ty pos _ ->
                return (Ref ref ty pos)
                
            , fSymRef = \ var ty pos _ -> do
                let ref = SymbolicRef var ty pos
                initializeSymbolicRef ref
                return ref
                
            , fCond = \ guard0 true0 false0 ty pos thread -> do
                guard1 <- guard0 thread
                true1  <- true0 thread
                false1 <- false0 thread
                return (Conditional guard1 true1 false1 ty pos) }

substituteQuantifier :: ([Expression] -> Expression) -> Identifier -> Identifier -> Identifier -> (Thread -> Engine r Expression) -> RuntimeType -> Position -> Thread -> Engine r Expression
substituteQuantifier quantifier element range domain formula _ _ thread0 = do
    ref <- readVar thread0 domain
    processRef ref
        (\ concRef -> do
            structure <- dereference concRef
            let (ArrayValue values) = structure
            formulas <- mapM (\ (value, index) -> do
                state <- getLocal
                thread1 <- writeVar thread0 element value
                thread2 <- writeVar thread1 range index
                f <- substitute thread2 =<< formula thread2
                putLocal state
                return f
                ) ((zip values . map (lit' . intLit')) [0..])
            return $ quantifier formulas)
        (const (throw (InternalError "substituteQuantifier: Symbolic Reference")))
        infeasible

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
            = writeVar threadN name =<< substitute evaluationThread value

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
    tid <- (1 +) <$> getNumberOfForks
    let pc         = context cfg entry
    let parameters = member ^?! SL.params -- TODO: does not work for constructors or non-static
    debug "Spawning initial thread"
    newThread <- pushInitialStackFrame (Thread tid (-1) pc T.empty T.empty) member (zip parameters arguments)
    modifyLocal (\ state -> state & (threads %~ S.insert newThread) & (numberOfForks +~ 1))
    return newThread

spawn :: Thread -> Node -> DeclarationMember -> [Expression] -> Engine r Thread
spawn parent entry member arguments = do
    cfg <- askCFG
    childTid <- (1 +) <$> getNumberOfForks
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

childrenOf :: Thread -> Engine r (S.Set Thread)
childrenOf thread
    = S.filter (\ thread' -> thread' ^. parent == thread ^. tid) <$> getThreads