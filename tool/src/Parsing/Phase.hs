module Parsing.Phase(
    parsingPhase
) where

import Polysemy
import Polysemy.Error
import Polysemy.Reader
import Data.Error
import Data.Configuration
import Logger
import Parsing.Lexer
import Parsing.Parser
import Parsing.Labeling
import Language.Syntax    (CompilationUnit)
import Text.Pretty

parsingPhase :: Members [Reader Configuration, Error ErrorMessage, Trace, Embed IO] r 
    => Sem r CompilationUnit
parsingPhase = do
    Configuration{fileName} <- ask
    inform "Starting the Parsing Phase"
    input <- embed (readFile fileName)
    lexed <- fromEither (lexer fileName input)
    parsed <- fromEither (parser fileName lexed)
    debug (toString parsed)
    inform "Parsing Phase succeeded"
    labelize parsed