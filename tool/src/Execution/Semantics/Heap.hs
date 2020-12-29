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
import           Control.Lens ((&), (^?!), (^.), (%~), (.~), element, Field2(_2), _head, _tail)
import           Data.Positioned
import           Analysis.Type.Typeable
import           Execution.Effects
import           Execution.Errors
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

dereference :: ExecutionState -> Reference -> Maybe HeapValue
dereference state ref = Heap.lookup ref (state ^. heap)

--------------------------------------------------------------------------------
-- Object handling
--------------------------------------------------------------------------------

writeConcreteField :: ExecutionState -> Reference -> Identifier -> Expression -> Engine r ExecutionState
writeConcreteField state ref field value =
    case dereference state ref of
        Just (ObjectValue values ty) -> do
            debug ("Assigning '" ++ toString value ++ "' to '" ++ toString ref ++ "." ++ toString field ++ "'")
            let newStructure = ObjectValue (M.insert field value values) ty
            return $ state & (heap %~ Heap.insert ref newStructure)
        Just (ArrayValue values) ->
             stop state (expectedObjectErrorMessage "writeConcreteField")
        Nothing ->
            stop state (uninitializedReferenceErrorMessage "writeConcreteField" ref)
            
writeSymbolicField :: ExecutionState -> Expression -> Identifier -> Expression -> Engine r ExecutionState
writeSymbolicField state2 ref@SymbolicRef{} field value =
    case AliasMap.lookup (ref ^?! SL.var) (state2 ^. aliasMap) of
        Just aliases -> 
            if S.size aliases == 1
                then do
                    let (Ref concRef _ _) = S.elemAt 0 aliases
                    writeConcreteField state2 concRef field value
                else 
                    foldM writeSymbolicAliasField state2 aliases
        Nothing -> 
            stop state2 (noAliasesErrorMessage "writeSymbolicField")
    where
        writeSymbolicAliasField :: ExecutionState -> Expression -> Engine r ExecutionState
        writeSymbolicAliasField stateN alias@Ref{} = do
            oldValue <- readConcreteField stateN (alias ^?! SL.ref) field
            let newValue = conditional' (ref `equal'` alias) value oldValue
            writeConcreteField stateN (alias ^?! SL.ref) field newValue

writeSymbolicField _ (Lit NullLit{} _ _) _ _ = 
    infeasible

writeSymbolicField state value _ _ =
    stop state (expectedSymbolicReferenceErrorMessage "writeSymbolicField" value)

readConcreteField :: ExecutionState -> Reference -> Identifier -> Engine r Expression
readConcreteField state ref field = 
    case dereference state ref of 
        Just (ObjectValue values _) ->
            case values M.!? field of
                Just value@SymbolicRef{} -> 
                    case AliasMap.lookup (value ^?! SL.var) (state ^. aliasMap) of
                        Just aliases ->
                            if S.size aliases == 1
                                then return $ S.elemAt 0 aliases
                                else return value
                        Nothing ->
                            return value
                Just value ->
                    return value
                Nothing -> 
                    stop state (readOfUndeclaredFieldErrorMessge "readConcreteField" field)
        Just (ArrayValue values) ->
             stop state (expectedObjectErrorMessage "readConcreteField")
        Nothing ->
            stop state (uninitializedReferenceErrorMessage "readConcreteField" ref)

readSymbolicField :: ExecutionState -> Expression -> Identifier -> Engine r Expression
readSymbolicField state2 ref@SymbolicRef{} field =
    case AliasMap.lookup (ref ^?! SL.var) (state2 ^. aliasMap) of
        Just aliases -> do
            options <- mapM (readSymbolicAliasField state2) (S.toList aliases)
            return $ foldr (\ (concRef, value) -> conditional' (ref `equal'` concRef) value) (head options ^. _2) (tail options)
        Nothing      -> stop state2 (noAliasesErrorMessage "readSymbolicField")
    where
        readSymbolicAliasField :: ExecutionState -> Expression -> Engine r (Expression, Expression)
        readSymbolicAliasField stateN ref = (ref, ) <$> readConcreteField stateN (ref ^?! SL.ref) field

readSymbolicField state ref _ =
    stop state (expectedReferenceErrorMessage "readSymbolicField" ref)

--------------------------------------------------------------------------------
-- Array handling
--------------------------------------------------------------------------------

sizeof :: ExecutionState -> Reference -> Engine r Int
sizeof state ref = case dereference state ref of
    Just (ArrayValue elements) ->
        return (length elements)
    Just ObjectValue{} ->
        stop state (expectedArrayErrorMessage "sizeof")
    Nothing ->
        stop state (uninitializedReferenceErrorMessage "sizeof" ref)

readConcreteElem :: ExecutionState -> Reference -> Int -> Engine r Expression
readConcreteElem state ref index = case dereference state ref of
    Just (ArrayValue values) 
        | index >= 0 && index < length values -> return (values ^?! element index)
        | otherwise -> infeasible
    Just ObjectValue{} ->
        stop state (expectedArrayErrorMessage "readConcreteElem")
    Nothing ->
        stop state (uninitializedReferenceErrorMessage "readConcreteElem" ref)
        
readSymbolicElem :: ExecutionState -> Reference -> Expression -> Engine r Expression
readSymbolicElem state ref index = case dereference state ref of
    Just (ArrayValue values) -> do
        let indices = map (lit' . intLit') [1..]
        let value = foldr (\ (value, concIndex) -> conditional' (index `equal'` concIndex) value) (values ^?! _head) (zip (values ^?! _tail) indices)
        return value
    Just ObjectValue{} ->
        stop state (expectedArrayErrorMessage "readSymbolicElem")
    Nothing ->
        stop state (uninitializedReferenceErrorMessage "readSymbolicElem" ref)

writeConcreteElem :: ExecutionState -> Reference -> Int -> Expression -> Engine r ExecutionState
writeConcreteElem state ref index value = case dereference state ref of
    Just (ArrayValue values)
        | index >= 0 && index < length values -> do
            let newStructure = ArrayValue (values & (element index .~ value))            
            return $ state & (heap %~ Heap.insert ref newStructure)
        | otherwise -> 
            infeasible
    Just ObjectValue{} ->
        stop state (expectedArrayErrorMessage "writeConcreteElem")
    Nothing ->             
        stop state (uninitializedReferenceErrorMessage "writeConcreteElem" ref)

writeSymbolicElem :: ExecutionState -> Reference -> Expression -> Expression -> Engine r ExecutionState
writeSymbolicElem state ref index value = case dereference state ref of
    Just (ArrayValue values) -> do
        let indices = map (lit' . intLit') [0..]
        let newStructure = ArrayValue $ zipWith (\ oldValue concIndex -> conditional' (index `equal'` concIndex) value oldValue) values indices
        return $ state & (heap %~ Heap.insert ref newStructure)
    Just ObjectValue{} ->
        stop state (expectedArrayErrorMessage "writeSymbolicElem")
    Nothing ->
        stop state  (uninitializedReferenceErrorMessage "writeSymbolicElem" ref)