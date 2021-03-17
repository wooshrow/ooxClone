module Execution.State.Heap(
      Heap
    , HeapValue(..)
    , lookup
    , insert
    , size
) where

import           Prelude                     hiding (lookup)
import qualified Data.Map               as M
import qualified Text.Pretty as Pretty
import           Language.Syntax
import           Analysis.Type.Typeable

newtype Heap = Heap { unHeap :: M.Map Reference HeapValue }
    deriving (Show, Eq, Ord)
    
instance Semigroup Heap where
    (Heap a) <> (Heap b) = Heap (a <> b)

instance Monoid Heap where
    mempty = Heap M.empty

instance Pretty.Pretty Heap where
    pretty = Pretty.pretty . unHeap

data HeapValue 
    = ObjectValue (M.Map Identifier Expression) RuntimeType
    | ArrayValue  [Expression]
    deriving (Show, Eq, Ord)

instance Typeable HeapValue where
    typeOf (ObjectValue _ ty) = ty
    typeOf (ArrayValue es)    = ArrayRuntimeType (typeOf (head es))

instance Pretty.Pretty HeapValue where
    pretty (ObjectValue fields ty) = 
        Pretty.pretty fields <> 
        Pretty.text ":" <> Pretty.pretty ty
        
    pretty (ArrayValue values)     = 
        Pretty.pretty values <> 
        Pretty.text ":" <> Pretty.pretty (typeOf (head values)) <> Pretty.text "[]"

lookup :: Reference -> Heap -> Maybe HeapValue
lookup ref = M.lookup ref . unHeap

insert :: Reference -> HeapValue -> Heap -> Heap
insert ref value = Heap . M.insert ref value . unHeap

size :: Heap -> Int
size = M.size . unHeap