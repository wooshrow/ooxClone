module Execution.State.InterleavingConstraints where

import           Prelude hiding ((<>))
import qualified Data.Set as S
import           Text.Pretty
import           Analysis.CFA.CFG
import           Language.Syntax

type InterleavingConstraints = [InterleavingConstraint]

data InterleavingConstraint
    = IndependentConstraint    CFGContext CFGContext
    | NotIndependentConstraint CFGContext CFGContext
    deriving (Show, Eq, Ord)

instance Pretty [InterleavingConstraint] where
    pretty = commas

instance Pretty InterleavingConstraint where
    pretty (IndependentConstraint (_, v, _, _) (_, w, _, _)) 
        = pretty v <> text "~" <> pretty w
    pretty (NotIndependentConstraint (_, v, _, _) (_, w, _, _)) 
        = pretty v <> text "!~" <> pretty w

type ReadWriteSet = (S.Set Reference, S.Set Reference)