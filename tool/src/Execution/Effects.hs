module Execution.Effects(
      module Polysemy
    , module Polysemy.Error
    , module Polysemy.Cache
    , module Polysemy.State
    , module Polysemy.NonDet
    , module Polysemy.Reader
    , module Logger
    , Engine
    , infeasible
    , stop
    , invalid
    , deadlock
    , unknown
    , finish
    , askConfig
    , askCFG
    , askTable
) where

import Polysemy
import Polysemy.Error
import Polysemy.Cache
import Polysemy.State
import Polysemy.NonDet
import Polysemy.Reader
import Control.Applicative
import Control.Lens ((^.), Field1(_1), Field2(_2), Field3(_3))
import Logger
import Execution.State
import Data.Configuration
import Data.Positioned
import Text.Pretty (toDebugString)
import Data.Statistics
import Analysis.CFA.CFG
import Analysis.SymbolTable
import Execution.Result
import Language.Syntax

type Engine r a = Members [ Reader (Configuration, ControlFlowGraph, SymbolTable)
                          , Error VerificationResult
                          , Cache Expression
                          , State Statistics
                          , NonDet
                          , Trace
                          , Embed IO] r => Sem r a

-- | Terminate the current branch.
infeasible :: Engine r a
infeasible = measurePrune >> empty

-- | Terminate with an internal error.
stop :: ExecutionState -> String -> Engine r a
stop state message = do
    debug ("Stopping with state: \n" ++ toDebugString state)
    throw $ InternalError message

-- | Terminate with an invalid expression.
invalid :: ExecutionState -> Expression -> Engine r a
invalid state expression = throw $ Invalid (getPos expression) (state ^. programTrace)

-- | Terminate with a deadlock.
deadlock :: ExecutionState -> Engine r a
deadlock state = throw $ Deadlock (state ^. programTrace)

-- | Terminate with an unknown result
unknown :: ExecutionState -> Expression -> Engine r a
unknown state expression = throw $ Unknown (getPos expression) (state ^. programTrace)

-- | Terminate the current branch.
finish :: ExecutionState -> Engine r a
finish state = do
    let trace = reverse $ map (^. _2) (state ^. programTrace)
    inform ("Finished program path: " ++ toDebugString trace)
    measureFinish 
    empty

-- | Retrieve the current Configuration.
askConfig :: Engine r Configuration
askConfig = (^. _1) <$> ask

-- | Retrieve the Control Flow Graph.
askCFG :: Engine r ControlFlowGraph
askCFG = (^. _2) <$> ask

-- | Retrieve the Symbol Table.
askTable :: Engine r SymbolTable
askTable = (^. _3) <$> ask
