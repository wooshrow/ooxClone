module Execution.State where
    
import qualified Data.Set                 as S
import           Data.Foldable
import           Control.Lens
import           Polysemy           
import           Polysemy.Reader
import           Polysemy.Error 
import           Polysemy.State
import           Polysemy.Cache
import           Execution.State.PathConstraints
import           Execution.State.LockSet
import           Execution.State.Thread
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
    , _remainingK              :: !Int 
    }

$(makeLenses ''ExecutionState)

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
-- Effect utilities
--------------------------------------------------------------------------------

infeasible :: Engine r a
infeasible = measurePrune >> throw Infeasible

finish :: Engine r ()
finish = measureFinish

haltInfeasible :: VerificationResult -> Engine r ()
haltInfeasible Infeasible = return ()
haltInfeasible e          = throw e

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
