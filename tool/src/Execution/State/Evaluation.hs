module Execution.State.Evaluation where

import Language.Syntax

-- | An Evaluation Result is either a (simplified) expression or the result type.
type EvaluationResult a = Either Expression a