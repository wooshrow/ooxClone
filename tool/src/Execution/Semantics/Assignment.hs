module Execution.Semantics.Assignment(
      execLhs
    , execRhs
) where

import qualified Data.Set as S
import           Control.Monad (foldM)
import           Control.Monad.Extra
import           Control.Lens ((^?!), (^.))
import           Text.Pretty
import           Analysis.Type.Typeable
import           Execution.State
import           Execution.State.Heap
import           Execution.State.AliasMap as AliasMap
import           Execution.Effects
import           Execution.Errors
import           Execution.Semantics.Evaluation
import           Execution.Semantics.Heap
import           Execution.Semantics.StackFrame
import           Execution.Semantics.Concretization
import           Language.Syntax
import qualified Language.Syntax.Lenses as SL

execLhs :: ExecutionState -> Lhs -> Expression -> Engine r ExecutionState
execLhs state0 lhs@LhsVar{} value =
    writeDeclaration state0 (lhs ^?! SL.var) value

execLhs state0 lhs@LhsField{} value = do
    let field = lhs ^?! SL.field
    ref <- readDeclaration state0 (lhs ^?! SL.var)
    case ref of
        Lit NullLit{} _ _ -> 
            infeasible
        Ref{} -> 
            writeConcreteField state0 (ref ^?! SL.ref) field value
        SymbolicRef{} -> do
            state1 <- initializeSymbolicRef state0 ref
            state2 <- removeSymbolicNull state1 ref
            writeSymbolicField state2 ref field value
        _ -> 
            stop state0 (expectedReferenceErrorMessage "execLhs" ref)
            
execLhs state0 lhs@LhsElem{} value = do
    ref <- readDeclaration state0 (lhs ^?! SL.var)
    case ref of
        Lit NullLit{} _ _ -> 
            infeasible
        Ref{} -> do
            (state1, index) <- evaluateAsInt state0 (lhs ^?! SL.index)
            writeElem state1 (ref ^?! SL.ref) index value
        SymbolicRef symRef _ _ -> do
            (state1, concretizations) <- concretesOfType state0 ARRAYRuntimeType ref
            concretize concretizations state1 $ \ state2 ->
                case AliasMap.lookup symRef (state2 ^. aliasMap) of
                    Just refs 
                        | length refs == 1 -> do
                            (state3, index) <- evaluateAsInt state2 (lhs ^?! SL.index)
                            let (Ref ref _ _) = S.elemAt 0 refs
                            writeElem state3 ref index value
                        | otherwise ->
                            stop state2 (exactlyOneAliasErrorMessage "execLhs" (length refs))
                    Nothing -> 
                        stop state2 (noAliasesErrorMessage "execLhs")
        _ ->
            stop state0 (expectedReferenceErrorMessage "execLhs" ref)

execRhs :: ExecutionState -> Rhs -> Engine r (ExecutionState, Expression)
execRhs state0 rhs@RhsExpression{} = do
    (state1, concretizations) <- concretesOfType state0 ARRAYRuntimeType (rhs ^?! SL.value)
    concretizeWithResult concretizations state1 $ \ state2 -> do
        (state3, value) <- evaluate state2 (rhs ^?! SL.value)
        debug ("Evaluated rhs '" ++ toString rhs ++ "' to '" ++ toString value ++ "'")
        return (state3, value)

execRhs state rhs@RhsField{} = do
    ref <- readDeclaration state (rhs ^?! SL.var ^?! SL.var)
    execRhsField state ref (rhs ^?! SL.field)
  
execRhs state0 rhs@RhsElem{} = do
    let var = rhs ^?! SL.var ^?! SL.var
    ref <- readDeclaration state0 var
    case ref of
        Lit NullLit{} _ _ -> 
            infeasible
        SymbolicRef ref _ _ -> 
            case AliasMap.lookup ref (state0 ^. aliasMap) of
                Just aliases
                    | length aliases == 1 -> do
                        let alias = S.elemAt 0 aliases
                        debug ("Overwritting '" ++ toString var ++ "' with single alias '" ++ toString alias ++ "'")
                        state1 <- writeDeclaration state0 var alias
                        execRhs state1 rhs
                    | otherwise -> 
                        stop state0 (exactlyOneAliasErrorMessage "execRhs" (S.size aliases))
                Nothing -> 
                    stop state0 (noAliasesErrorMessage "execRhs")
        Ref{} -> do
            (state1, index) <- evaluateAsInt state0 (rhs ^?! SL.index)
            value <- readElem state1 (ref ^?! SL.ref) index
            return (state1, value)
        _ ->
            stop state0 (expectedReferenceErrorMessage "execRhs" ref)

execRhs state0 rhs@RhsArray{} = do 
    (state1, sizes) <- mapAccumM evaluateAsInt state0 (rhs ^?! SL.sizes)
    execNewArray state1 sizes (typeOf rhs)

execRhs state RhsCall{} =
    stop state (expectedNoMethodCallErrorMessage "execRhs")

execRhsField :: ExecutionState -> Expression -> Identifier -> Engine r (ExecutionState, Expression)
execRhsField state0 (Conditional guard true0 false0 ty info) field =  do
    (state1, true1)  <- execRhsField state0 true0 field
    (state2, false1) <- execRhsField state1 false0 field
    return (state2, Conditional guard true1 false1 ty info)

execRhsField _ (Lit NullLit{} _ _) _ = 
    infeasible

execRhsField state (Ref ref _ _) field = 
    (state,) <$> readConcreteField state ref field

execRhsField state0 ref@SymbolicRef{} field = do
    state1 <- initializeSymbolicRef state0 ref
    state2 <- removeSymbolicNull state1 ref
    (state1,) <$> readSymbolicField state2 ref field

execRhsField state ref _ =
    stop state (expectedReferenceErrorMessage "execRhsField" ref)

execNewArray :: ExecutionState -> [EvaluationResult Int] -> RuntimeType -> Engine r (ExecutionState, Expression)
execNewArray state [Right size] ty = do
    let elemTy    = ty ^?! SL.innerTy
    let structure = ArrayValue (replicate size (defaultValue elemTy))
    allocate state structure

execNewArray state0 (Right size:sizes) ty = do
    (state1, refs) <- foldM (const . execNewInnerArray) (state0, []) [0..size-1]
    let structure = ArrayValue refs
    allocate state1 structure
    where
        execNewInnerArray :: (ExecutionState, [Expression]) -> Engine r (ExecutionState, [Expression])
        execNewInnerArray (state1, refs) = do
            let elemTy = ty ^?! SL.innerTy
            (state2, ref) <- execNewArray state1 sizes elemTy
            return (state2, ref : refs)

execNewArray state (Left size:_) _ =
    stop state (expectedConcreteValueErrorMessge "execNewArray" size)
