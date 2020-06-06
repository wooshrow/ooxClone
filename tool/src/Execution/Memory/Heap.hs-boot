module Execution.Memory.Heap where
    import qualified Data.Map        as M
    import           Language.Syntax

    data HeapValue 
        = ObjectValue (M.Map Identifier Expression) RuntimeType
        | ArrayValue  [Expression]

    type Heap = M.Map Reference HeapValue  