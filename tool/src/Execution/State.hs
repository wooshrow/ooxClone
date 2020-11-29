module Execution.State where
    
import           Prelude hiding ((<>))
import qualified Data.Set                 as S
import           Data.Foldable
import           Control.Lens
import           Execution.State.PathConstraints
import           Execution.State.LockSet
import           Execution.State.Thread
import           Execution.State.AliasMap
import           Execution.State.Heap
import           Execution.State.InterleavingConstraints
import           Text.Pretty
import           Analysis.CFA.CFG
import           Analysis.Type.Typeable
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

getThread :: ExecutionState -> ThreadId -> Maybe Thread
getThread state tid' = find (\ thread -> thread ^. tid == tid') (state ^. threads)

getCurrentThread :: ExecutionState -> Maybe Thread
getCurrentThread state = do
    currentTid <- state ^. currentThreadId
    getThread state currentTid

