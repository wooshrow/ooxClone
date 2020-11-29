module Execution.Phase where

import Polysemy
import Polysemy.Error
import Polysemy.State
import Polysemy.Reader
import Logger
import Data.Configuration
import Data.Error
import Data.Statistics
import Analysis.CFA.CFG
import Analysis.SymbolTable
import Execution.Engine
import Verification.Result
import Text.Pretty

executionPhase :: Members [Error ErrorMessage, State Statistics, Trace, Reader Configuration, Embed IO] r 
    => SymbolTable -> ControlFlowGraph -> Sem r VerificationResult
executionPhase table cfg = do
    inform "Starting the Symbolic Execution Phase"
    result <- execute table cfg
    inform (toString result)
    inform "Symbolic Execution Phase succeeded"
    return result