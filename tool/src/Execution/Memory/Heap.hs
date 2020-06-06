module Execution.Memory.Heap where

import qualified Data.Map                 as M
import qualified Data.Set                 as S
import           Data.Maybe
import           Control.Lens hiding (index, indices)
import           Control.Monad
import           Polysemy.Reader
import           Polysemy.Error
import           Text.Pretty
import           Execution.ExecutionState
import           Execution.Memory.AliasMap
import           Analysis.SymbolTable
import           Data.Positioned
import           Data.Configuration
import           Language.Syntax
import           Language.Syntax.DSL
import           Language.Syntax.Pretty()
import qualified Language.Syntax.Lenses    as SL
import           Analysis.Type.Typeable
import           Verification.Result

--------------------------------------------------------------------------------
-- The Heap Type
--------------------------------------------------------------------------------

type Heap = M.Map Reference HeapValue

data HeapValue 
    = ObjectValue (M.Map Identifier Expression) RuntimeType
    | ArrayValue  [Expression]
    deriving (Show)

instance Typeable HeapValue where
    typeOf (ObjectValue _ ty)  = ty
    typeOf (ArrayValue values) = ArrayRuntimeType (typeOf (head values))

--------------------------------------------------------------------------------
-- High level Heap functions
--------------------------------------------------------------------------------

dereference :: Reference -> Engine r HeapValue
dereference ref = do
    structure <- (M.!? ref) <$> getHeap
    maybe (segfault ref) return structure
    
allocate :: HeapValue -> Engine r Expression
allocate value = do
    ref <- newRef <$> getHeap
    modifyLocal (\ state -> state & (heap %~ M.insert ref value))
    return $ Ref ref (typeOf value) unknownPos

processRef :: Expression -> (Reference -> Engine r a) -> (Expression -> Engine r a) -> Engine r a -> Engine r a
processRef expression onConcRef onSymRef onNull
    = case expression of
        Ref concRef _ _ -> onConcRef concRef
        SymbolicRef symRef _ _ -> do -- Initialize sym. ref here?
            aliases <- fromJust <$> getAliases symRef
            if S.size aliases == 1
                then case S.findMin aliases of
                        Ref concRef _ _   -> onConcRef concRef
                        Lit NullLit{} _ _ -> onNull
                        _                 -> throw (InternalError "processRef: not a reference in alias map")
                else onSymRef expression
        Lit NullLit{} _ _ -> onNull
        _                 -> throw (InternalError ("processRef: " ++ toString expression))
    
--------------------------------------------------------------------------------
-- Field Reading

readField :: Reference -> Identifier -> Engine r Expression
readField ref field = do
    structure <- dereference ref
    let (ObjectValue values _) = structure
    case values M.!? field of
        Just value -> return value
        Nothing    -> throw (InternalError ("readField: " ++ toString field))

readSymbolicField :: Expression -> Identifier -> Engine r Expression
readSymbolicField var@(SymbolicRef symRef _ _) field = do
    concretes <- fromJust <$> getAliasesWithoutNull symRef
    values <- mapM ( \ concRef@(Ref ref _ _) -> (concRef, ) <$> readField ref field) (S.toList concretes)
    return $ foldr (\ (concRef, value) -> conditional' (var `equal'` concRef) value) (head values ^. _2) (tail values)

--------------------------------------------------------------------------------
-- Field Writing

writeField :: Reference -> Identifier -> Expression -> Engine r ()
writeField ref field value = do
    structure <- dereference ref
    let (ObjectValue values ty) = structure
    let newStructure = ObjectValue (M.insert field value values) ty
    modifyLocal (\ state -> state & (heap %~ M.insert ref newStructure))

writeSymbolicField :: Expression -> Expression -> Identifier -> Expression -> Engine r ()
writeSymbolicField symRef concRef@(Ref ref _ _) field value = do
    oldValue <- readField ref field
    writeField ref field (conditional' (symRef `equal'` concRef) value oldValue)
writeSymbolicField _ (Lit NullLit{} _ _) _ _ = infeasible

--------------------------------------------------------------------------------
-- Array Writing

createArray :: [EvaluationResult Int] -> RuntimeType -> Engine r Expression
createArray [Right size] ty = do
    let elemTy    = ty ^?! SL.innerTy
    let structure = ArrayValue (replicate size (defaultValue elemTy))
    allocate structure
createArray (Right size:sizes) ty = do
    let elemTy = ty ^?! SL.innerTy
    refs <- replicateM size (createArray sizes elemTy)
    let structure = ArrayValue refs
    allocate structure
createArray [Left size] _ = 
    throw (InternalError ("createArray: symbolic size '" ++ toString size ++ "'"))

writeIndex :: Reference -> Int -> Expression -> Engine r ()
writeIndex ref index value = do
    size <- arraySize ref
    when (index >= size || index < 0)
        infeasible
    structure <- dereference ref
    let (ArrayValue values) = structure
    let newStructure = ArrayValue (values & (element index .~ value))
    modifyLocal (\ state -> state & (heap %~ M.insert ref newStructure))

writeIndexSymbolic :: Reference -> Expression -> Expression -> Engine r ()
writeIndexSymbolic ref symIndex value = do
    structure <- dereference ref
    let (ArrayValue values) = structure
    let indices = map (lit' . intLit') [0..]
    let newStructure = ArrayValue $ map (\ (oldValue, concIndex) -> conditional' (symIndex `equal'` concIndex) value oldValue) (zip values indices)
    modifyLocal (\ state -> state & (heap %~ M.insert ref newStructure))

--------------------------------------------------------------------------------
-- Array Reading

arraySize :: Reference -> Engine r Int
arraySize ref = do
    structure <- dereference ref
    let (ArrayValue values) = structure
    return (length values)

readIndex :: Reference -> Int -> Engine r Expression
readIndex ref index = do
    size <- arraySize ref
    when (index >= size || index < 0)
        infeasible
    structure <- dereference ref
    let (ArrayValue values) = structure
    return (values ^?! element index)

readIndexSymbolic :: Reference -> Expression -> Engine r Expression
readIndexSymbolic ref symIndex = do
    structure <- dereference ref
    let (ArrayValue values) = structure
    let indices = map (lit' . intLit') [1..]
    let value = foldr (\ (value', concIndex) -> conditional' (symIndex `equal'` concIndex) value') (values ^?! _head) (zip (values ^?! _tail) indices)
    return value

--------------------------------------------------------------------------------
-- Lazy Symbolic Reference Initialization
--------------------------------------------------------------------------------

initializeSymbolicRef :: Expression -> Engine r ()
initializeSymbolicRef var@(SymbolicRef symRef ty _) = do
    isInitialized <- M.member symRef <$> getAliasMap
    unless isInitialized $ do
        debug ("Initializing symbolic reference '" ++ toString var ++ "'")
        if ty `isOfType` ARRAYRuntimeType
            then initializeSymbolicArrays var
            else initializeSymbolicObject var
            
initializeSymbolicArrays :: Expression -> Engine r ()
initializeSymbolicArrays var@(SymbolicRef symRef ty _) = do
    config <- askConfig
    let arraySizes = [1..symbolicArraySize config]
    concRefs <- S.fromList <$> mapM (initializeSymbolicArray var) arraySizes
    aliases <- if symbolicAliases config
        then otherAliasesOfType ty
        else return S.empty
    let nullCase  = if symbolicNulls config then S.singleton (lit' nullLit') else S.empty
    let concretes = concRefs `S.union` aliases `S.union` nullCase
    setAliases symRef concretes

initializeSymbolicArray :: Expression -> Int -> Engine r Expression
initializeSymbolicArray (SymbolicRef symRef ty _) size = do
    let elemTy  = ty ^?! SL.innerTy    
    let indices = [0..size - 1]
    structure <- ArrayValue <$> mapM (initializeSymbolicIndex symRef elemTy) indices
    allocate structure

initializeSymbolicIndex :: Identifier -> RuntimeType -> Int -> Engine r Expression
initializeSymbolicIndex _ ty index = do
    sIndex <- newRef <$> getHeap
    let symName = Identifier (show sIndex ++ show index) unknownPos
    return $ createSymbolicVar symName ty

initializeSymbolicObject :: Expression -> Engine r ()
initializeSymbolicObject (SymbolicRef symRef ty _) = do
    (config, _, table) <- ask
    let fields = (S.toList . S.map getMember . getAllFields (ty ^?! SL.ty)) table
    values <- mapM initializeSymbolicField fields
    let structure = ObjectValue (M.fromList values) ty
    concRef <- allocate structure
    aliases <- if symbolicAliases config
        then otherAliasesOfType ty
        else return S.empty
    let nullCase  = if symbolicNulls config then S.singleton (lit' nullLit') else S.empty
    let concretes = S.insert concRef (aliases `S.union` nullCase)
    setAliases symRef concretes

initializeSymbolicField :: DeclarationMember -> Engine r (Identifier, Expression)
initializeSymbolicField field = do
    sIndex <- newRef <$> getHeap
    let fieldName@(Identifier oldName pos) = field ^?! SL.name
    let symName = Identifier (oldName ++ show sIndex) pos 
    let value = createSymbolicVar symName (typeOf field)
    return (fieldName, value)

createSymbolicVar :: Typeable a => Identifier -> a -> Expression
createSymbolicVar (Identifier name pos) ty
    | ty `isOfType` REFRuntimeType = SymbolicRef (Identifier ('_' : name) pos) (typeOf ty) pos
    | otherwise                    = SymbolicVar (Identifier ('_' : name) pos) (typeOf ty) pos

--------------------------------------------------------------------------------
-- Low level heap operations
--------------------------------------------------------------------------------

newRef :: Heap -> Reference
newRef heap = 1 + M.size heap
                   
segfault :: Reference -> Engine r HeapValue
segfault ref = throw (InternalError ("segfault on reference '" ++ show ref ++ "'"))
