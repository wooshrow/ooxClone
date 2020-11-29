module Execution.Semantics.Concretization where
    import Execution.Effects (Engine)
    import Execution.State (ExecutionState)
    import Language.Syntax (Expression)

    initializeSymbolicRef :: ExecutionState -> Expression -> Engine r ExecutionState
