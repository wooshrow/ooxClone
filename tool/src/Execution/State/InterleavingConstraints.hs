module Execution.State.InterleavingConstraints where

import           Prelude hiding ((<>))
import           Text.Pretty
import           Analysis.CFA.CFG
import           Execution.State.Thread

type InterleavingConstraints = [InterleavingConstraint]

data InterleavingConstraint
    = IndependentConstraint    (ThreadId,CFGContext) (ThreadId,CFGContext)
    | NotIndependentConstraint (ThreadId,CFGContext) (ThreadId,CFGContext)
    deriving (Show, Eq, Ord)

instance Pretty [InterleavingConstraint] where
    pretty = commas

instance Pretty InterleavingConstraint where
    pretty (IndependentConstraint (t1,(_, v, _, _)) (t2,(_, w, _, _)))
        = pretty t1 <> text ":" <> pretty v
          <> text " ~ "
          <> pretty t2 <> text ":" <> pretty w
    pretty (NotIndependentConstraint (t1,(_, v, _, _)) (t2,(_, w, _, _)))
        = pretty t1 <> text ":" <> pretty v
          <> text " !~ "
          <> pretty t2 <> text ":" <> pretty w
