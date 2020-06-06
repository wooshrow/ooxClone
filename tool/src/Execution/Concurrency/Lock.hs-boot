module Execution.Concurrency.Lock where
    import qualified Data.Map        as M
    import           Language.Syntax

    type LockSet = M.Map Reference Int