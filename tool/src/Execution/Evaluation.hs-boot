module Execution.Evaluation(
      evaluateAsInt
    , evaluateAsBool
    , evaluate
) where

import Execution.State
import Execution.State.Thread
import Language.Syntax

evaluateAsInt :: Thread -> Expression -> Engine r (EvaluationResult Int)

evaluateAsBool :: Thread -> Expression -> Engine r (EvaluationResult Bool)

evaluate :: Thread -> Expression -> Engine r Expression