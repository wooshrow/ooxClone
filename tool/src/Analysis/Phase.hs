module Analysis.Phase(
    analysisPhase
) where

import           Polysemy
import           Polysemy.Error
import           Polysemy.State
import           Polysemy.Reader
import qualified Data.Map                   as M (empty)
import           Data.Graph.Inductive.Graph as G (empty)
import           Data.Configuration
import           Data.Error
import           Text.Pretty
import           Analysis.SymbolTable
import           Analysis.Type.Analysis
import           Analysis.CFA.Analysis
import           Analysis.CFA.CFG
import           Syntax.Syntax

analysisPhase :: Members [Reader Configuration, Error ErrorMessage, Embed IO] r 
    => CompilationUnit -> Sem r (SymbolTable, ControlFlowGraph)
analysisPhase program = do
    inform "Starting the Analysis Phase"
    let table = constructSymbolTable program
    cfg <- runReader table (do
        program' <- evalState M.empty (typeCompilationUnit program)
        fst <$> runState G.empty (constructCompilationUnit program'))
    debug (toString cfg)
    inform "Analysis Phase succeeded"
    return (table, cfg)