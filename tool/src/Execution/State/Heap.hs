module Execution.State.Heap(
      Heap
    , HeapValue(..)
    , lookup
    , insert
    , size
) where

import           Prelude                     hiding (lookup)
import qualified Data.Map               as M
import           Language.Syntax
import           Analysis.Type.Typeable

newtype Heap = Heap { unHeap :: M.Map Reference HeapValue }

instance Semigroup Heap where
    (Heap a) <> (Heap b) = Heap (a <> b)

instance Monoid Heap where
    mempty = Heap M.empty

data HeapValue 
    = ObjectValue (M.Map Identifier Expression) RuntimeType
    | ArrayValue  [Expression]
    deriving (Show)

instance Typeable HeapValue where
    typeOf (ObjectValue _ ty) = ty
    typeOf (ArrayValue es)    = typeOf $ head es

lookup :: Reference -> Heap -> Maybe HeapValue
lookup ref = M.lookup ref . unHeap

insert :: Reference -> HeapValue -> Heap -> Heap
insert ref value = Heap . M.insert ref value . unHeap

size :: Heap -> Int
size = M.size . unHeap