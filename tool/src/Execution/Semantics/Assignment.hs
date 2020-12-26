module Execution.Semantics.Assignment(
      execLhs
    , execRhs
) where

import qualified Data.Set as S
import           Control.Monad
import           Control.Monad.Extra
import           Control.Lens ((^?!), (^.))
import           Text.Pretty
import           Analysis.Type.Typeable
import           Execution.State
import           Execution.State.Heap
import           Execution.State.AliasMap as AliasMap
import           Execution.Effects
import           Execution.Semantics.Evaluation
import           Execution.Semantics.Heap
import           Execution.Semantics.StackFrame
import           Execution.Semantics.Concretization
import           Language.Syntax
import qualified Language.Syntax.Lenses as SL

execLhs :: ExecutionState -> Lhs -> Expression -> Engine r ExecutionState
execLhs state0 lhs@LhsVar{} value =
    writeDeclaration state0 (lhs ^?! SL.var) value

execLhs state lhs@LhsField{} value = do
    let field = lhs ^?! SL.field
    ref <- readDeclaration state (lhs ^?! SL.var)
    case ref of
        Lit NullLit{} _ _ -> infeasible
        Ref ref _ _       -> writeConcreteField state ref field value
        SymbolicRef{}     -> writeSymbolicField state ref field value
        _                 -> stop state "execLhs: expected a reference"
            
execLhs state0 lhs@LhsElem{} value = do
    ref <- readDeclaration state0 (lhs ^?! SL.var)
    case ref of
        Lit NullLit{} _ _ -> 
            infeasible
        Ref ref _ _       -> do
            (state1, index) <- evaluateAsInt state0 (lhs ^?! SL.index)
            case index of
                Right index -> writeConcreteElem state1 ref index value
                Left index  -> writeSymbolicElem state1 ref index value
        SymbolicRef symRef _ _ -> do
            (state1, concretizations) <- concretesOfType state0 ARRAYRuntimeType ref
            concretize concretizations state1 $ \ state2 ->
                case AliasMap.lookup symRef (state2 ^. aliasMap) of
                    Nothing   -> 
                        stop state2 "execLhs: No concrete references"
                    Just refs ->
                        if length refs /= 1
                            then stop state2 "execLhs: Non concretized symbolic reference"
                            else do
                                (state3, index) <- evaluateAsInt state2 (lhs ^?! SL.index)
                                let (Ref ref _ _) = S.elemAt 0 refs
                                case index of
                                    Right index -> writeConcreteElem state3 ref index value
                                    Left index  -> writeSymbolicElem state3 ref index value
        _                      ->
            stop state0 "execLhs: expected a reference"

execRhs :: ExecutionState -> Rhs -> Engine r (ExecutionState, Expression)
execRhs state0 rhs@RhsExpression{} = do
    (state1, concretizations) <- concretesOfType state0 ARRAYRuntimeType (rhs ^?! SL.value)
    concretizeWithResult concretizations state1 $ \ state2 -> do
        (state3, value) <- evaluate state2 (rhs ^?! SL.value)
        debug ("Evaluated rhs '" ++ toString rhs ++ "' to '" ++ toString value ++ "'")
        return (state3, value)

execRhs state0 rhs@RhsField{} = do
    let field = rhs ^?! SL.field
    ref <- readDeclaration state0 (rhs ^?! SL.var ^?! SL.var)
    case ref of
        Lit NullLit{} _ _ -> 
            infeasible
        Ref ref _ _ -> do
            value <- readConcreteField state0 ref field
            return (state0, value)
        SymbolicRef{} -> do
            state1 <- initializeSymbolicRef state0 ref
            value  <- readSymbolicField state1 ref field
            return (state1, value)
        Conditional{} -> 
            execRhsFieldConditional state0 ref field
        _ -> 
            stop state0 "execRhs: expected a reference or conditional"
        where
            execRhsFieldConditional :: ExecutionState -> Expression -> Identifier -> Engine r (ExecutionState, Expression)
            execRhsFieldConditional state0 (Conditional guard true0 false0 ty info) field = do
                (state1, true1)  <- execRhsFieldConditional state0 true0 field
                (state2, false1) <- execRhsFieldConditional state1 false0 field
                return (state2, Conditional guard true1 false1 ty info)
            execRhsFieldConditional _ (Lit NullLit{} _ _) _ = 
                infeasible
            execRhsFieldConditional state0 (Ref ref _ _) field = 
                (state0,) <$> readConcreteField state0 ref field
            execRhsFieldConditional state0 ref@SymbolicRef{} field = do
                state1 <- initializeSymbolicRef state0 ref
                (state1,) <$> readSymbolicField state0 ref field
            execRhsFieldConditional _ _ _ =
                stop state0 "execRhsFieldConditional: expected a reference or conditional"
  
execRhs state0 rhs@RhsElem{} = do
    let var = rhs ^?! SL.var ^?! SL.var
    ref <- readDeclaration state0 var
    case ref of
        Lit NullLit{} _ _   -> 
            infeasible
        SymbolicRef ref _ _ -> 
            case AliasMap.lookup ref (state0 ^. aliasMap) of
                Nothing      -> 
                    stop state0 "execRhs: no aliases"
                Just aliases -> 
                    if S.size aliases /= 1
                        then stop state0 "execRhs: Symbolic Reference"
                        else do
                            let alias = S.elemAt 0 aliases
                            debug ("Overwritting '" ++ toString var ++ "' with single alias '" ++ toString alias ++ "'")
                            state1 <- writeDeclaration state0 var alias
                            execRhs state1 rhs
        Ref ref _ _         -> do
            (state1, index) <- evaluateAsInt state0 (rhs ^?! SL.index)
            value <- either (readSymbolicElem state1 ref) (readConcreteElem state1 ref) index
            return (state1, value)
        _                   ->
            stop state0 "execRhs: expected a reference"

execRhs state0 rhs@RhsArray{} = do 
    (state1, sizes) <- mapAccumM evaluateAsInt state0 (rhs ^?! SL.sizes)
    execNewArray state1 sizes (typeOf rhs)

execRhs state RhsCall{} =
    stop state "execRhs: evaluating a method call"

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

execNewArray state _ _ =
    stop state "execNewArray: symbolic size"