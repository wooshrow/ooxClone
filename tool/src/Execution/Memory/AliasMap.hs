module Execution.Memory.AliasMap where

import qualified Data.Map                 as M
import qualified Data.Set                 as S
import           Data.Maybe              
import           Polysemy.LocalState
import           Control.Lens
import           Execution.ExecutionState
import           Data.Configuration
import           Text.Pretty
import           Analysis.Type.Typeable
import           Language.Syntax
import           Language.Syntax.DSL
import           Language.Syntax.Pretty()

type AliasMap = M.Map Identifier (S.Set Expression)

type Concretization = M.Map Identifier Expression

-- | Returns the set of all aliases of the given symbolic reference.
getAliases :: Identifier -> Engine r (Maybe (S.Set Expression))
getAliases var = do
    concretes <- getAliasMap
    return (concretes M.!? var)

-- | Returns the set (excluding null) of all aliases of the given symbolic reference.
getAliasesWithoutNull :: Identifier -> Engine r (Maybe (S.Set Expression))
getAliasesWithoutNull var = do
    mConcretes <- getAliases var
    case mConcretes of
        Just concretes -> do
            let concretes' = S.delete (lit' nullLit') concretes
            modifyLocal (\ state -> state & (aliasMap %~ M.insert var concretes'))
            return $ Just concretes'
        Nothing -> return Nothing

-- | Returns the set of all other concrete references of the given type.
otherAliasesOfType :: RuntimeType -> Engine r (S.Set Expression)
otherAliasesOfType ty = S.unions . M.elems . M.filter (any (`isOfType` ty)) <$> getAliasMap

-- | Sets the aliases of the given symbolic reference.
setAliases :: Identifier -> S.Set Expression -> Engine r ()
setAliases var newAliases = do
    oldAliases <- fromMaybe S.empty <$> getAliases var
    debug ("Updating the alias map entry of '" ++ toString var ++ "' from '" ++ toString oldAliases ++ "' to '" ++ toString newAliases ++ "'")
    modifyLocal (\ state -> state & (aliasMap %~ M.insert var newAliases))