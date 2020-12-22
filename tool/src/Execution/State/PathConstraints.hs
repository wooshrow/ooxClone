module Execution.State.PathConstraints(
      PathConstraints
    , singleton
    , insert
    , asExpression
) where

import qualified Data.HashSet as H 
import qualified Text.Pretty as Pretty
import           Language.Syntax.DSL
import           Language.Syntax (Expression)

newtype PathConstraints = PathConstraints { unPathConstraints :: H.HashSet Expression }
    deriving (Show)
    
instance Semigroup PathConstraints where
    (PathConstraints a) <> (PathConstraints b) = PathConstraints (a <> b)

instance Monoid PathConstraints where
    mempty = PathConstraints mempty

instance Pretty.Pretty PathConstraints where
    pretty = Pretty.pretty . unPathConstraints

singleton :: Expression -> PathConstraints
singleton = PathConstraints . H.singleton

-- TODO: split insertions as conjuctions?
insert :: Expression -> PathConstraints -> PathConstraints
insert c = PathConstraints . H.insert c . unPathConstraints

asExpression :: PathConstraints -> Expression
asExpression = ands' . unPathConstraints