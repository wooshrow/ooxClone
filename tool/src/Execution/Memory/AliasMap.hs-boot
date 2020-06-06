module Execution.Memory.AliasMap where
    import qualified Data.Map      as M
    import qualified Data.Set      as S
    import           Language.Syntax
    
    type AliasMap = M.Map Identifier (S.Set Expression)