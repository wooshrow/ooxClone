module Execution.Concurrency.POR where
    import Analysis.CFA.CFG

    type InterleavingConstraints = [InterleavingConstraint]
    
    data InterleavingConstraint
        = IndependentConstraint    CFGContext CFGContext
        | NotIndependentConstraint CFGContext CFGContext