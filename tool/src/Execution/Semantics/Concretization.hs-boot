module Execution.Semantics.Concretization where
    import Execution.State (Engine, ExecutionState)
    import Language.Syntax (Expression)

    initializeSymbolicRef :: ExecutionState -> Expression -> Engine r ExecutionState
