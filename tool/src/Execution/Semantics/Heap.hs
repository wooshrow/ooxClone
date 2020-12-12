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
import           Execution.State
import           Execution.State.Heap as Heap
import           Execution.State.AliasMap as AliasMap
import {-# SOURCE #-} Execution.Semantics.Concretization (initializeSymbolicRef, removeSymbolicNull)
import           Language.Syntax
import           Language.Syntax.DSL
import qualified Language.Syntax.Lenses as SL
import           Language.Syntax.Fold

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
             stop state ("writeConcreteField: array value '" ++ toString values ++ "'")
        Nothing ->
            stop state ("writeConcreteField: dereference of uninitialized ref '" ++ toString ref ++ "'")


writeSymbolicField :: ExecutionState -> Expression -> Identifier -> Expression -> Engine r ExecutionState
writeSymbolicField state0 ref@SymbolicRef{} field value = do
    state1 <- initializeSymbolicRef state0 ref
    state2 <- removeSymbolicNull state1 ref
    case AliasMap.lookup (ref ^?! SL.var) (state2 ^. aliasMap) of
        Just aliases -> 
            if S.size aliases == 1
                then do
                    let (Ref concRef _ _) = S.elemAt 0 aliases
                    writeConcreteField state2 concRef field value
                else 
                    foldM writeSymbolicAliasField state2 aliases
        Nothing -> 
            stop state2 "writeSymbolicField: no aliases"
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
                    stop state ("readConcreteField: " ++ toString field)
        Just (ArrayValue values) ->
             stop state ("readConcreteField: array value '" ++ toString values ++ "'")
        Nothing ->
            stop state ("readConcreteField: dereference of uninitialized ref '" ++ toString ref ++ "'")

readSymbolicField :: ExecutionState -> Expression -> Identifier -> Engine r Expression
readSymbolicField state0 ref@SymbolicRef{} field = do
    state1 <- initializeSymbolicRef state0 ref
    state2 <- removeSymbolicNull state1 ref
    case AliasMap.lookup (ref ^?! SL.var) (state2 ^. aliasMap) of
        Just aliases -> do
            options <- mapM (readSymbolicAliasField state2) (S.toList aliases)
            return $ foldr (\ (concRef, value) -> conditional' (ref `equal'` concRef) value) (head options ^. _2) (tail options)
        Nothing      -> stop state2 "readSymbolicField: no aliases"
    where
        readSymbolicAliasField :: ExecutionState -> Expression -> Engine r (Expression, Expression)
        readSymbolicAliasField stateN ref = (ref, ) <$> readConcreteField stateN (ref ^?! SL.ref) field

readSymbolicField state _ _ =
    stop state "readSymbolicField: non-reference"

--------------------------------------------------------------------------------
-- Array handling
--------------------------------------------------------------------------------

sizeof :: ExecutionState -> Reference -> Engine r Int
sizeof state ref =
    case dereference state ref of
        Just (ArrayValue values) ->
            return (length values)
        Nothing ->
            stop state ("sizeof: dereference of uninitialized ref '" ++ toString ref ++ "'")

readConcreteElem :: ExecutionState -> Reference -> Int -> Engine r Expression
readConcreteElem state ref index = 
    case dereference state ref of
        Just (ArrayValue values) ->
            if index >= 0 && index < length values
                then return (values ^?! element index)
                else infeasible
        Nothing ->
            stop state ("readConcreteElem: dereference of uninitialized ref '" ++ toString ref ++ "'")

readSymbolicElem :: ExecutionState -> Reference -> Expression -> Engine r Expression
readSymbolicElem state ref index =
    case dereference state ref of
        Just (ArrayValue values) -> do
            let indices = map (lit' . intLit') [1..]
            let value = foldr (\ (value, concIndex) -> conditional' (index `equal'` concIndex) value) (values ^?! _head) (zip (values ^?! _tail) indices)
            return value
        Nothing ->
            stop state ("readSymbolicElem: dereference of uninitialized ref '" ++ toString ref ++ "'")

writeConcreteElem :: ExecutionState -> Reference -> Int -> Expression -> Engine r ExecutionState
writeConcreteElem state ref index value =
    case dereference state ref of
        Just (ArrayValue values) ->
            if index >= 0 && index < length values
                then do
                    let newStructure = ArrayValue (values & (element index .~ value))            
                    return $ state & (heap %~ Heap.insert ref newStructure)
                else 
                    infeasible
        Nothing ->             
            stop state ("writeConcreteElem: dereference of uninitialized ref '" ++ toString ref ++ "'")

writeSymbolicElem :: ExecutionState -> Reference -> Expression -> Expression -> Engine r ExecutionState
writeSymbolicElem state ref index value =
    case dereference state ref of
        Just (ArrayValue values) -> do
            let newStructure = ArrayValue $ zipWith (\ oldValue concIndex -> conditional' (index `equal'` concIndex) value oldValue) values indices
            return $ state & (heap %~ Heap.insert ref newStructure)
        Nothing ->
            stop state ("writeSymbolicElem: dereference of uninitialized ref '" ++ toString ref ++ "'")
    where
        indices = map (lit' . intLit') [0..]

--------------------------------------------------------------------------------
-- Auxiliary functions
--------------------------------------------------------------------------------

segfault :: ExecutionState -> Reference -> Engine r HeapValue
segfault state ref = stop state ("segfault on reference '" ++ show ref ++ "'")