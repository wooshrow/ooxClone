module Execution.Semantics.Heap(
      allocate
    , dereference
    , writeConcreteField
    , writeSymbolicField
    , readConcreteField
    , readSymbolicField
    , sizeof
    , readConcreteElem
    , readSymbolicElem
    , writeConcreteElem
    , writeSymbolicElem
) where

import           Prelude hiding (lookup)
import           Control.Monad
import qualified Data.Map as M
import qualified Data.Set as S
import           Text.Pretty
import           Control.Lens
import           Data.Positioned
import           Analysis.Type.Typeable
import           Execution.State
import           Execution.State.Heap as Heap
import           Execution.State.AliasMap as AliasMap
import           Language.Syntax
import           Language.Syntax.DSL
import qualified Language.Syntax.Lenses as SL

allocate :: ExecutionState -> HeapValue -> Engine r (ExecutionState, Expression)
allocate state0 structure = do
    let ref    = size (state0 ^. heap) + 1
    let state1 = state0 & (heap %~ Heap.insert ref structure)
    let value  = Ref ref (typeOf structure) unknownPos
    return (state1, value)

dereference :: ExecutionState -> Reference -> Engine r HeapValue
dereference state ref = do
    let structure = Heap.lookup ref (state ^. heap)
    maybe (segfault state ref) return structure

--------------------------------------------------------------------------------
-- Object handling
--------------------------------------------------------------------------------

writeConcreteField :: ExecutionState -> Reference -> Identifier -> Expression -> Engine r ExecutionState
writeConcreteField state ref field value = do
    structure <- dereference state ref
    case structure of
        ObjectValue values ty -> do
            let newStructure = ObjectValue (M.insert field value values) ty
            return $ state & (heap %~ Heap.insert ref newStructure)
        ArrayValue values     ->
             stop state ("writeConcreteField: array value '" ++ toString values ++ "'")

writeSymbolicField :: ExecutionState -> Expression -> Identifier -> Expression -> Engine r ExecutionState
writeSymbolicField state ref@SymbolicRef{} field value =
    -- TODO: originally, null was removed from the alias map
    case AliasMap.lookup (ref ^?! SL.var) (state ^. aliasMap) of
        Just aliases -> foldM writeSymbolicAliasField state (S.filter (/= lit' nullLit') aliases)
        Nothing      -> stop state "writeSymbolicField: no aliases"
    where
        writeSymbolicAliasField :: ExecutionState -> Expression -> Engine r ExecutionState
        writeSymbolicAliasField stateN alias@Ref{} = do
            oldValue <- readConcreteField stateN (alias ^?! SL.ref) field
            let newValue = conditional' (ref `equal'` alias) value oldValue
            writeConcreteField stateN (alias ^?! SL.ref) field newValue

writeSymbolicField _ (Lit NullLit{} _ _) _ _ = 
    infeasible

writeSymbolicField state _ _ _ =
    stop state "writeSymbolicField: non-reference"

readConcreteField :: ExecutionState -> Reference -> Identifier -> Engine r Expression
readConcreteField state ref field = do
    structure <- dereference state ref
    case structure of 
        ObjectValue values _ ->
            case values M.!? field of
                Just value -> return value
                Nothing    -> stop state ("readConcreteField: " ++ toString field)
        ArrayValue values    ->
             stop state ("readConcreteField: array value '" ++ toString values ++ "'")

readSymbolicField :: ExecutionState -> Expression -> Identifier -> Engine r Expression
readSymbolicField state ref@SymbolicRef{} field = 
    -- TODO: originally, null was removed from the alias map
    case AliasMap.lookup (ref ^?! SL.var) (state ^. aliasMap) of
        Just aliases -> do
            options <- mapM readSymbolicAliasField (S.toList (S.filter (/= lit' nullLit') aliases))
            return $ foldr (\ (concRef, value) -> conditional' (ref `equal'` concRef) value) (head options ^. _2) (tail options)
        Nothing      -> stop state "readSymbolicField: no aliases"
    where
        readSymbolicAliasField :: Expression -> Engine r (Expression, Expression)
        readSymbolicAliasField ref = (ref, ) <$> readConcreteField state (ref ^?! SL.ref) field

readSymbolicField state _ _ =
    stop state "readSymbolicField: non-reference"

--------------------------------------------------------------------------------
-- Array handling
--------------------------------------------------------------------------------

sizeof :: ExecutionState -> Reference -> Engine r Int
sizeof state ref = do
    structure <- dereference state ref
    let (ArrayValue values) = structure
    return (length values)

readConcreteElem :: ExecutionState -> Reference -> Int -> Engine r Expression
readConcreteElem state ref index = do
    structure <- dereference state ref
    let (ArrayValue values) = structure
    if index >= 0 && index < length values
        then return (values ^?! element index)
        else infeasible

readSymbolicElem :: ExecutionState -> Reference -> Expression -> Engine r Expression
readSymbolicElem = undefined

writeConcreteElem :: ExecutionState -> Reference -> Int -> Expression -> Engine r ExecutionState
writeConcreteElem state ref index value = do
    structure <- dereference state ref
    let (ArrayValue values) = structure
    if index >= 0 && index < length values
        then do
            let newStructure = ArrayValue (values & (element index .~ value))            
            return $ state & (heap %~ Heap.insert ref newStructure)
        else 
            infeasible

writeSymbolicElem :: ExecutionState -> Reference -> Expression -> Expression -> Engine r ExecutionState
writeSymbolicElem state ref index value = do
    structure <- dereference state ref
    let (ArrayValue values) = structure
    let newStructure = ArrayValue $ map (\ (oldValue, concIndex) -> conditional' (index `equal'` concIndex) value oldValue) (zip values indices)
    return $ state & (heap %~ Heap.insert ref newStructure)
    where
        indices = map (lit' . intLit') [0..]

--------------------------------------------------------------------------------
-- Auxiliary functions
--------------------------------------------------------------------------------

segfault :: ExecutionState -> Reference -> Engine r HeapValue
segfault state ref = stop state ("segfault on reference '" ++ show ref ++ "'")

{-

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

-}