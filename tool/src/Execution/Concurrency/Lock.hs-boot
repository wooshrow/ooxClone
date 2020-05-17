module Execution.Concurrency.Lock where
    import qualified Data.Map      as M
    import           Syntax.Syntax

    type LockSet = M.Map Reference Int