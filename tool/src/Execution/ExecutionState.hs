{-# LANGUAGE ScopedTypeVariables, TypeOperators #-}

module Execution.ExecutionState where
    
import qualified Data.Map                 as M
import qualified Data.Set                 as S
import qualified Data.HashSet             as H
import           Data.Foldable
import           Control.Lens
import           Control.Monad
import           Control.Monad.Extra
import           Polysemy              
import           Polysemy.Internal.Combinators
import           Polysemy.Reader
import           Polysemy.Error 
import           Polysemy.State
import           Polysemy.Cache
import {-# SOURCE #-} Execution.Memory.Heap
import {-# SOURCE #-} Execution.Memory.AliasMap
import {-# SOURCE #-} Execution.Concurrency.Thread
import {-# SOURCE #-} Execution.Concurrency.Lock
import {-# SOURCE #-} Execution.Concurrency.POR
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
    , _constraints             :: H.HashSet Expression
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
emptyState = ExecutionState { _threads                 = S.empty
                            , _constraints             = H.empty
                            , _heap                    = M.empty
                            , _aliasMap                = M.empty
                            , _locks                   = M.empty
                            , _numberOfForks           = 0
                            , _interleavingConstraints = []
                            , _programTrace            = []
                            , _remainingK              = 0 }

type Engine r a = Members [ Reader (Configuration, ControlFlowGraph, SymbolTable)
                          , Error VerificationResult
                          , Cache Expression
                          , LocalState ExecutionState
                          , State Statistics
                          , Embed IO] r => Sem r a

defaultValue :: Typeable a => a -> Expression
defaultValue ty = lit' $ 
    case typeOf ty of
        UIntRuntimeType  -> uIntLit'  0   ; IntRuntimeType         -> intLit'  0
        FloatRuntimeType -> floatLit' 0.0 ; BoolRuntimeType        -> boolLit' False
        CharRuntimeType  -> charLit'  "\0"; ReferenceRuntimeType{} -> nullLit'
        ARRAYRuntimeType -> nullLit'      ; ArrayRuntimeType{}     -> nullLit'

-- | An Evaluation Result is either a (simplified) expression or the result type.
type EvaluationResult a = Either Expression a

--------------------------------------------------------------------------------
-- Effect utilities
--------------------------------------------------------------------------------

data LocalState s m a where
    PutLocal :: s -> LocalState s m ()
    GetLocal :: LocalState s m s

makeSem ''LocalState

modifyLocal :: Member (LocalState s) r => (s -> s) -> Sem r ()
modifyLocal f = do
    s <- getLocal
    putLocal $! f s

runLocalState :: s -> Sem (LocalState s ': r) a -> Sem r (s, a)
runLocalState = stateful $ \case
    GetLocal   -> \ s -> pure (s, s)
    PutLocal s -> const $ pure (s, ())

evalLocalState :: s -> Sem (LocalState s ': r) a -> Sem r a
evalLocalState s = fmap snd . runLocalState s

branch :: Foldable t => (a -> Engine r b) -> t a -> Engine r [b]
branch f options = do
    measureBranches options
    foldM' (\ acc value -> do
        s <- getLocal
        result <- catch ((:[]) <$> f value) (\ e -> [] <$ haltInfeasible e)
        putLocal s
        return $ result ++ acc) [] (toList options)

branch_ :: Foldable t => (a -> Engine r ()) -> t a -> Engine r ()
branch_ f = void . branch f

infeasible :: Engine r a
infeasible = measurePrune >> throw Infeasible

finish :: Engine r ()
finish = measureFinish

haltInfeasible :: VerificationResult -> Engine r ()
haltInfeasible Infeasible = return ()
haltInfeasible e          = throw e

getThreads :: Engine r (S.Set Thread)
getThreads = (^. threads) <$> getLocal

getConstraints :: Engine r (H.HashSet Expression)
getConstraints = (^. constraints) <$> getLocal

getHeap :: Engine r Heap
getHeap = (^. heap) <$> getLocal

getAliasMap :: Engine r AliasMap
getAliasMap = (^. aliasMap) <$> getLocal

getLocks :: Engine r LockSet
getLocks = (^. locks) <$> getLocal

getNumberOfForks :: Engine r Int
getNumberOfForks = (^. numberOfForks) <$> getLocal

getInterleavingConstraints :: Engine r InterleavingConstraints
getInterleavingConstraints = (^. interleavingConstraints) <$> getLocal

getProgramTrace :: Engine r [CFGContext]
getProgramTrace = (^. programTrace) <$> getLocal

getRemainingK :: Engine r Int
getRemainingK = (^. remainingK) <$> getLocal

askConfig :: Engine r Configuration
askConfig = (^. _1) <$> ask

askCFG :: Engine r ControlFlowGraph
askCFG = (^. _2) <$> ask

askTable :: Engine r SymbolTable
askTable = (^. _3) <$> ask
