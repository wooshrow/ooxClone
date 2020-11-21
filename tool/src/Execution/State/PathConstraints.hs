module Execution.State.PathConstraints(
      PathConstraints
    , singleton
    , insert
    , asExpression
) where

import qualified Data.HashSet        as H 
import           Language.Syntax.DSL
import           Language.Syntax          (Expression)

newtype PathConstraints = PathConstraints { unPathConstraints :: H.HashSet Expression }
    deriving (Show)
    
instance Semigroup PathConstraints where
    (PathConstraints a) <> (PathConstraints b) = PathConstraints (a <> b)

instance Monoid PathConstraints where
    mempty = PathConstraints mempty

singleton :: Expression -> PathConstraints
singleton = PathConstraints . H.singleton

insert :: Expression -> PathConstraints -> PathConstraints
insert c = PathConstraints . H.insert c . unPathConstraints

asExpression :: PathConstraints -> Expression
asExpression = ands' . unPathConstraints