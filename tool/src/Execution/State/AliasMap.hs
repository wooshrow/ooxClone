module Execution.State.AliasMap(
      AliasMap
    , lookup
    , member
    , elems
    , insert
    , filter
    , Concretization
) where
    
import           Prelude hiding (lookup, filter)
import qualified Data.Map        as M
import qualified Data.Set        as S
import           Language.Syntax

newtype AliasMap = AliasMap { unAliasMap :: M.Map Identifier (S.Set Expression) }

instance Semigroup AliasMap where
    (AliasMap a) <> (AliasMap b) = AliasMap (a <> b)

instance Monoid AliasMap where
    mempty = AliasMap M.empty

lookup :: Identifier -> AliasMap -> Maybe (S.Set Expression)
lookup var = M.lookup var . unAliasMap

member :: Identifier -> AliasMap -> Bool
member var = M.member var . unAliasMap

elems :: AliasMap -> [S.Set Expression]
elems = M.elems . unAliasMap

insert :: Identifier -> S.Set Expression -> AliasMap -> AliasMap
insert var concretes = AliasMap . M.insert var concretes . unAliasMap

filter :: (S.Set Expression -> Bool) -> AliasMap -> AliasMap
filter f = AliasMap . M.filter f . unAliasMap

type Concretization = M.Map Identifier Expression
