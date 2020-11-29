module Execution.State where
    
import           Prelude hiding ((<>))
import qualified Data.Set                 as S
import           Data.Foldable
import           Data.Positioned
import           Control.Lens
import           Control.Applicative (empty)
import           Polysemy           
import           Polysemy.Reader
import           Polysemy.Error 
import           Polysemy.NonDet 
import           Polysemy.State
import           Polysemy.Cache
import Text.Pretty (($+$), (<>), text, Pretty(toString, pretty))
import           Execution.State.PathConstraints
import           Execution.State.LockSet
import Execution.State.Thread ( tid, Thread, ThreadId )
import           Execution.State.AliasMap
import           Execution.State.Heap
import           Execution.State.InterleavingConstraints
import           Data.Configuration
import           Data.Statistics
import           Analysis.CFA.CFG
import           Analysis.SymbolTable
import           Analysis.Type.Typeable
import           Verification.Result
import           Language.Syntax
import           Language.Syntax.DSL

data ExecutionState = ExecutionState
    { _threads                 :: S.Set Thread
    , _currentThreadId         :: Maybe ThreadId
    , _constraints             :: PathConstraints
    , _heap                    :: !Heap
    , _aliasMap                :: !AliasMap
    , _locks                   :: !LockSet
    , _numberOfForks           :: !Int
    , _interleavingConstraints :: !InterleavingConstraints
    , _programTrace            :: ![CFGContext]
    , _remainingK              :: !Int }
    deriving (Show)

$(makeLenses ''ExecutionState)

instance Pretty ExecutionState where
    pretty state =
        text "threads="     <> pretty (state ^. threads)         $+$
        text "tid="         <> pretty (state ^. currentThreadId) $+$
        text "constraints=" <> pretty (state ^. constraints)     $+$
        text "heap="        <> pretty (state ^. heap)            $+$
        text "aliases="     <> pretty (state ^. aliasMap)        $+$
        text "locks="       <> pretty (state ^. locks)

emptyState :: ExecutionState
emptyState = ExecutionState { _threads                 = mempty
                            , _currentThreadId         = Nothing
                            , _constraints             = mempty
                            , _heap                    = mempty
                            , _aliasMap                = mempty
                            , _locks                   = mempty
                            , _numberOfForks           = 0
                            , _interleavingConstraints = mempty
                            , _programTrace            = mempty
                            , _remainingK              = 0 }

type Engine r a = Members [ Reader (Configuration, ControlFlowGraph, SymbolTable)
                          , Error VerificationResult
                          , Cache Expression
                          , State Statistics
                          , NonDet
                          , Embed IO] r => Sem r a

updateThreadInState :: ExecutionState -> Thread -> ExecutionState
updateThreadInState state thread = state & (threads %~ S.insert thread)

defaultValue :: Typeable a => a -> Expression
defaultValue ty = lit' $ 
    case typeOf ty of
        UIntRuntimeType   -> uIntLit'  0   ; IntRuntimeType         -> intLit'  0
        FloatRuntimeType  -> floatLit' 0.0 ; BoolRuntimeType        -> boolLit' False
        CharRuntimeType   -> charLit'  "\0"; ReferenceRuntimeType{} -> nullLit'
        ARRAYRuntimeType  -> nullLit'      ; ArrayRuntimeType{}     -> nullLit'
        StringRuntimeType -> nullLit'      

--------------------------------------------------------------------------------
-- Exploration utility functions
--------------------------------------------------------------------------------

-- | Terminate the current branch.
infeasible :: Engine r a
infeasible = measurePrune >> empty -- throw Infeasible

-- | Terminate with an internal error.
stop :: ExecutionState -> String -> Engine r a
stop state message = do
    debug ("Stopping with state: \n" ++ toString state)
    throw $ InternalError message

-- | Terminate with an invalid expression.
invalid :: ExecutionState -> Expression -> Engine r a
invalid state expression = 
    throw $ Invalid (getPos expression) (state ^. programTrace)

-- | Terminate with a deadlock.
deadlock :: ExecutionState -> Engine r a
deadlock state = throw $ Deadlock (state ^. programTrace)

-- | Terminate the current branch.
finish :: Engine r a
finish = measureFinish >> empty

--------------------------------------------------------------------------------
-- Effect utilities
--------------------------------------------------------------------------------

getThread :: ExecutionState -> ThreadId -> Maybe Thread
getThread state tid' = find (\ thread -> thread ^. tid == tid') (state ^. threads)

getCurrentThread :: ExecutionState -> Maybe Thread
getCurrentThread state = do
    let allThreads = state ^. threads
    currentTid <- state ^. currentThreadId
    find (\ thread -> thread ^. tid == currentTid) allThreads

askConfig :: Engine r Configuration
askConfig = (^. _1) <$> ask

askCFG :: Engine r ControlFlowGraph
askCFG = (^. _2) <$> ask

askTable :: Engine r SymbolTable
askTable = (^. _3) <$> ask
