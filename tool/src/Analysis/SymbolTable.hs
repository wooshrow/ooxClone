module Analysis.SymbolTable (
      SymbolTable
    , Symbol
    , SymbolTableEntries
    , SymbolTableEntry
    , constructSymbolTable
    , lookupConfigurationString
    , matchMethod
    , lookupMethod
    , lookupConstructor
    , lookupField
    , getAllEntries
    , getAllMethods
    , getDeclaration
    , getMember
    , getAllFields
) where

import qualified Data.Map                 as M
import qualified Data.Set                 as S
import           Data.List                     (elemIndex)
import           Data.Positioned
import           Control.Lens.Extras
import           Language.Syntax
import           Language.Syntax.Pretty()
import qualified Language.Syntax.Lenses   as SL
import           Analysis.Type.Typeable

newtype SymbolTable 
    = SymbolTable { unSymbolTable :: M.Map Symbol SymbolTableEntries }

data Symbol 
    = MethodSymbol Identifier Identifier
    | FieldSymbol Identifier Identifier
    deriving (Show, Eq, Ord)

type SymbolTableEntries 
    = S.Set SymbolTableEntry

data SymbolTableEntry 
    = SymbolTableEntry Declaration DeclarationMember
    deriving (Show, Eq, Ord)

--------------------------------------------------------------------------------
-- Method and Constructor Retrieval
--------------------------------------------------------------------------------
   
lookupConfigurationString :: String -> SymbolTable -> Maybe SymbolTableEntries
lookupConfigurationString entryPoint table = do
    dotPosition <- elemIndex '.' entryPoint
    let (className, methodName) = splitAt dotPosition entryPoint
    lookupMethod (Identifier className unknownPos) (Identifier (tail methodName) unknownPos) table
        
matchMethod :: Identifier -> Identifier -> RuntimeType -> [RuntimeType] -> SymbolTable -> Maybe SymbolTableEntries
matchMethod className@(Identifier _ classPos) methodName retType argTypes table = do
    namedMethods <- lookupMethod className methodName table
    let matchingMethods = S.filter match namedMethods
    if null matchingMethods
        then Nothing
        else return matchingMethods
    where
        match (SymbolTableEntry _ Method{..})
            = retType `isOfType` _returnTy && length paramTypes == length argTypes
            && all (uncurry isOfType) (zip paramTypes argTypes)
            where
                paramTypes
                    | _isStatic = [ty | (Parameter ty _ _) <- _params]
                    | otherwise = ReferenceType className classPos : [ty | (Parameter ty _ _) <- _params]
        match (SymbolTableEntry _ c@Constructor{..})
            = retType `isOfType` typeOf c && length paramTypes == length argTypes
            && all (uncurry isOfType) (zip paramTypes argTypes)
            where
                paramTypes = [ty | (Parameter ty _ _) <- _params]
        match (SymbolTableEntry _ Field{})
            = error "matchMethod: unexpected field"

lookupConstructor :: Identifier -> SymbolTable -> Maybe SymbolTableEntries
lookupConstructor name = lookupMethod name name

lookupMethod :: Identifier -> Identifier -> SymbolTable -> Maybe SymbolTableEntries
lookupMethod className methodName (SymbolTable table)
    = table M.!? MethodSymbol className methodName

lookupField :: Identifier -> Identifier -> SymbolTable -> Maybe SymbolTableEntry
lookupField className fieldName (SymbolTable table)
    = S.findMin <$> table M.!? FieldSymbol className fieldName

getAllFields :: Identifier -> SymbolTable -> SymbolTableEntries
getAllFields className (SymbolTable table)
    = S.unions [entries | ((FieldSymbol name _), entries) <- M.toList table, name == className]

--------------------------------------------------------------------------------
-- Searching the Symbol Table
--------------------------------------------------------------------------------

getAllEntries :: SymbolTable -> [SymbolTableEntry]
getAllEntries (SymbolTable table) = concatMap (S.toList . snd) (M.toList table)

getAllMethods :: SymbolTable -> [SymbolTableEntry]
getAllMethods (SymbolTable table) 
    = (filter (\ (SymbolTableEntry _ m) -> SL._Method `is` m || SL._Constructor `is` m) 
    . concatMap (S.toList . snd) . M.toList) table

getDeclaration :: SymbolTableEntry -> Declaration
getDeclaration (SymbolTableEntry declaration _) = declaration

getMember :: SymbolTableEntry -> DeclarationMember
getMember (SymbolTableEntry _ member) = member

--------------------------------------------------------------------------------
-- Construction of the Symbol Table
--------------------------------------------------------------------------------

constructSymbolTable :: CompilationUnit -> SymbolTable
constructSymbolTable (CompilationUnit declarations _)
    = SymbolTable $ M.unionsWith S.union (map (unSymbolTable . gatherFromDeclaration) declarations)

gatherFromDeclaration :: Declaration -> SymbolTable
gatherFromDeclaration c@(Class _ members _)
    = SymbolTable $ M.unionsWith S.union (map (unSymbolTable . gatherFromMember c) members)

gatherFromMember :: Declaration -> DeclarationMember -> SymbolTable
gatherFromMember c@(Class className _ _) m@(Constructor name _ _ _ _ _)
    = SymbolTable $ M.singleton symbol entry
    where 
        symbol = MethodSymbol className name
        entry  = S.singleton $ SymbolTableEntry c m
gatherFromMember c@(Class className _ _) m@(Method _ _ name _ _ _ _ _)
    = SymbolTable $ M.singleton symbol entry
    where
        symbol = MethodSymbol className name
        entry  = S.singleton $ SymbolTableEntry c m
gatherFromMember c@(Class className _ _) f@(Field _ name _)
    = SymbolTable $ M.singleton symbol entry
    where
        symbol = FieldSymbol className name
        entry  = S.singleton $ SymbolTableEntry c f