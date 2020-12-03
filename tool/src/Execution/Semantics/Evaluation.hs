{-# LANGUAGE LiberalTypeSynonyms #-}

module Execution.Semantics.Evaluation(
      EvaluationResult
    , evaluateAsInt
    , evaluateAsBool
    , evaluate
) where

import qualified Data.Set as S
import           Control.Lens ((^.)) 
import           Control.Monad
import           Text.Pretty
import           Data.Configuration
import           Data.Statistics
import           Analysis.CFA.CFG
import           Analysis.SymbolTable
import           Logger
import           Execution.Semantics.StackFrame
import           Execution.Semantics.Heap
import           Execution.Semantics.Concretization
import           Execution.Effects
import           Execution.State
import           Execution.State.Heap
import           Execution.State.AliasMap as AliasMap
import           Execution.Result
import           Data.Positioned
import           Language.Syntax
import           Language.Syntax.Fold
import           Language.Syntax.DSL

-- | An Evaluation Result is either a (simplified) expression or the result type.
type EvaluationResult a = Either Expression a

evaluateAsInt :: ExecutionState -> Expression -> Engine r (ExecutionState, EvaluationResult Int)
evaluateAsInt state0 expression = do
    debug ("Evaluating '" ++ toString expression ++ "' as an int")
    (state1, result) <- evaluate state0 expression
    case result of
        (Lit (IntLit value _) _ _) -> measureLocalSolve >> return (state1, Right value)
        _                          -> return (state1, Left result)

evaluateAsBool :: ExecutionState -> Expression -> Engine r (ExecutionState, EvaluationResult Bool)
evaluateAsBool state0 expression = do
    debug ("Evaluating '" ++ toString expression ++ "' as a bool")
    (state1, result) <- evaluate state0 expression
    case result of
        (Lit (BoolLit value _) _ _) -> measureLocalSolve >> return (state1, Right value)
        _                           -> return (state1, Left result)

evaluate :: ExecutionState -> Expression -> Engine r (ExecutionState, Expression)
evaluate state expression = do
    applyLocalSolver <- applyLocalSolver <$> askConfig
    if applyLocalSolver
        then evaluate' state expression
        else substitute state expression

substitute :: ExecutionState -> Expression -> Engine r (ExecutionState, Expression)
substitute state0 expression = foldExpression algebra expression state0
    where
        algebra :: Members [ Reader (Configuration, ControlFlowGraph, SymbolTable)
                           , Error VerificationResult
                           , NonDet
                           , Cache Expression
                           , State Statistics
                           , Embed IO
                           , Trace] r => ExpressionAlgebra (ExecutionState -> Sem r (ExecutionState, Expression))
        algebra = ExpressionAlgebra
            { fForall = evaluateQuantifier ands'

            , fExists = evaluateQuantifier ors'
            
            , fBinOp = \ binOp lhs0 rhs0 ty pos state1 -> do
                (state2, lhs1) <- lhs0 state1
                (state3, rhs1) <- rhs0 state2
                return (state3, BinOp binOp lhs1 rhs1 ty pos)
                
            , fUnOp = \ unOp expr0 ty pos state1 -> do
                (state2, expr1) <- expr0 state1
                return (state2, UnOp unOp expr1 ty pos)
                
            , fVar = \ var _ _ state1 -> do
                ref <- readDeclaration state1 var
                case ref of
                    SymbolicRef{} -> do
                        state2 <- initializeSymbolicRef state1 ref
                        return (state2, ref)
                    _             -> 
                        return (state1, ref)
                
            , fSymVar = \ var ty pos state1 ->
                return (state1, SymbolicVar var ty pos)
                
            , fLit = \ lit ty pos state1 -> 
                return (state1, Lit lit ty pos)
                
            , fSizeOf = \ var _ _ state1 -> do
                ref <- readDeclaration state1 var
                case ref of 
                    Lit NullLit{} _ _ -> infeasible
                    SymbolicRef{}     -> stop state1 "substitute: SizeOf of symbolic reference"
                    Ref ref _ _       -> do
                        size <- fmap (lit' . intLit') (sizeof state1 ref)
                        return (state1, size)
                    _                 -> stop state1 "substitute: SizeOf of non-reference"

            , fRef = \ ref ty pos state1 ->
                return (state1, Ref ref ty pos)
                
            , fSymRef = \ var ty pos state1 -> do
                let ref = SymbolicRef var ty pos
                state2 <- initializeSymbolicRef state1 ref
                return (state2, ref)
                
            , fCond = \ guard0 true0 false0 ty pos state1 -> do
                (state2, guard1) <- guard0 state1
                (state3, true1)  <- true0 state2
                (state4, false1) <- false0 state3
                return (state4, Conditional guard1 true1 false1 ty pos) }

evaluate' :: ExecutionState -> Expression -> Engine r (ExecutionState, Expression)
evaluate' state0 expression = foldExpression algebra expression state0
    where
        algebra :: Members [ Reader (Configuration, ControlFlowGraph, SymbolTable)
                           , Error VerificationResult
                           , NonDet
                           , Cache Expression
                           , State Statistics
                           , Embed IO
                           , Trace] r => ExpressionAlgebra (ExecutionState -> Sem r (ExecutionState, Expression))
        algebra = ExpressionAlgebra
            { fForall = evaluateQuantifier ands'
            
            , fExists = evaluateQuantifier ors' 
           
            , fBinOp = \ binOp lhs0 rhs0 ty pos state1 -> do
                (state2, lhs1) <- lhs0 state1
                (state3, rhs1) <- rhs0 state2
                value <- evaluateBinOp state1 binOp lhs1 rhs1 ty pos
                return (state3, value)
                
            , fUnOp = \ unOp value0 ty pos state1 -> do
                (state2, value1) <- value0 state1
                value2 <- evaluateUnOp unOp value1 ty pos
                return (state2, value2)
                
            , fVar = \ var _ _ state1 -> do
                value <- readDeclaration state1 var
                return (state1, value)
                
            , fSymVar = \ var ty pos state1 -> 
                return (state1, SymbolicVar var ty pos)
                
            , fLit = \ lit ty pos state1 -> 
                return (state1, Lit lit ty pos)
                
            , fSizeOf = \ var _ _ state1 -> do
                ref <- readDeclaration state1 var
                case ref of
                    Lit NullLit {} _ _ -> 
                        infeasible
                    Ref ref _ _        -> do
                        size <- fmap (lit' . intLit') (sizeof state1 ref)
                        return (state1, size)
                    SymbolicRef ref _ _ ->
                        stop state1 "evaluate: SizeOf of symbolic reference"
                        {-case AliasMap.lookup ref (state1 ^. aliasMap) of
                            Just aliases -> if S.size aliases == 1
                                then 
                                    case S.elemAt 0 aliases of
                                        Lit NullLit{} _ _-> 
                                            infeasible
                                        Ref ref _ _      -> do
                                            size <- fmap (lit' . intLit') (sizeof state1 ref)
                                            return (state1, size)
                                        _                ->
                                            stop state1 "evaluate: no aliases"
                                else 
                                    stop state1 "evaluate: SizeOf of symbolic reference"
                            _            ->
                                stop state1 "evaluate: no aliases" -}
                    _                  -> stop state1 "evaluate: SizeOf of non-reference"

            , fRef = \ ref ty pos state1 -> 
                return (state1, Ref ref ty pos)
            
            , fSymRef = \ ref ty pos state1 -> do
                let expr = SymbolicRef ref ty pos
                state2 <- initializeSymbolicRef state1 expr
                return (state2, expr)
                
            , fCond = \ guard0 true0 false0 ty pos state1 -> do
                (state2, guard1) <- guard0 state1
                case guard1 of
                    (Lit (BoolLit True  _) _ _) -> true0 state2
                    (Lit (BoolLit False _) _ _) -> false0 state2
                    _                           -> do
                        (state3, true1)  <- true0 state2
                        (state4, false1) <- false0 state3
                        return (state4, Conditional guard1 true1 false1 ty pos ) }

evaluateQuantifier :: ([Expression] -> Expression) -> Identifier -> Identifier -> Identifier -> (ExecutionState -> Engine r (ExecutionState, Expression)) -> RuntimeType -> Position -> ExecutionState -> Engine r (ExecutionState, Expression)
evaluateQuantifier quantifier element range domain formula _ _ state0 = do
    ref <- readDeclaration state0 domain
    case ref of
        Lit NullLit{} _ _ -> infeasible
        SymbolicRef{}     -> stop state0 "evaluateQuantifier: symbolic reference"
        Ref ref _ _       -> do
            structure <- dereference state0 ref
            let (ArrayValue values) = structure
            let options = (zip values . map (lit' . intLit')) [0..]
            formulas <- forM options $ \ (value, index) -> do
                state1 <- writeDeclaration state0 element value
                state2 <- writeDeclaration state1 range index
                formula state2
            -- TODO: the alias map needs to be extracted from formulas and passed on
            evaluate state0 (quantifier (map snd formulas))

evaluateBinOp :: ExecutionState -> BinOp -> Expression -> Expression ->  RuntimeType -> Position -> Engine r Expression
-- Boolean Evaluation
evaluateBinOp state op (Lit (BoolLit a _) _ _) (Lit (BoolLit b _) _ _) ty pos = do
    lit <- case op of Implies  -> return (BoolLit (not a || b)); And   -> return (BoolLit (a && b))
                      Or       -> return (BoolLit (a || b))    ; Equal -> return (BoolLit (a == b))
                      NotEqual -> return (BoolLit (a /= b))    
                      _        -> stop state "evaluateBinOp: unsupported operator"
    return $ Lit (lit pos) ty pos

evaluateBinOp __ op expressionA@(Lit (BoolLit a _) _ _) expressionB ty pos =
    return $ case op of 
        Implies -> if a then expressionB else Lit (BoolLit True pos) ty pos
        And     -> if a then expressionB else Lit (BoolLit False pos) ty pos
        Or      -> if a then Lit (BoolLit True pos) ty pos else expressionB
        _       -> BinOp op expressionA expressionB ty pos
        
evaluateBinOp _ op expressionA expressionB@(Lit (BoolLit b _) _ _) ty pos =
    return $ case op of 
        Implies -> if b then Lit (BoolLit True pos) ty pos else UnOp Negate expressionA ty pos
        And     -> if b then expressionA else Lit (BoolLit False pos) ty pos
        Or      -> if b then Lit (BoolLit True pos) ty pos else expressionA
        _       -> BinOp op expressionA expressionB ty pos

-- Integer Evaluation
evaluateBinOp _ Divide _ (Lit (IntLit 0 _) _ _) _ _ = infeasible
evaluateBinOp _ Modulo _ (Lit (IntLit 0 _) _ _) _ _ = infeasible
evaluateBinOp state op (Lit (IntLit a _) _ _) (Lit (IntLit b _) _ _) ty pos = do
    lit <- case op of Equal       -> return (BoolLit (a == b))  ; NotEqual         -> return (BoolLit (a /= b))
                      LessThan    -> return (BoolLit (a < b))   ; LessThanEqual    -> return (BoolLit (a <= b))
                      GreaterThan -> return (BoolLit (a > b))   ; GreaterThanEqual -> return (BoolLit (a >= b))
                      Plus        -> return (IntLit  (a + b))   ; Minus            -> return (IntLit  (a - b))
                      Multiply    -> return (IntLit  (a * b))   ; Divide           -> return (IntLit (a `div` b))
                      Modulo      -> return (IntLit (a `mod` b))
                      _           -> stop state "evaluateBinOp: unsupported operator"
    return $ Lit (lit pos) ty pos

-- Reference Evaluation
evaluateBinOp state op (Ref a _ _) (Ref b _ _) ty pos = do
    lit <- case op of Equal    -> return (BoolLit (a == b)) 
                      NotEqual -> return (BoolLit (a /= b))
                      _        -> stop state "evaluateBinOp: unsupported operator"
    return $ Lit (lit pos) ty pos
    
evaluateBinOp state op Ref{} (Lit (NullLit _) _ _) ty pos = do
    lit <- case op of Equal    -> return (BoolLit False)
                      NotEqual -> return (BoolLit True)
                      _        -> stop state "evaluateBinOp: unsupported operator"
    return $ Lit (lit pos) ty pos

evaluateBinOp state op (Lit (NullLit _) _ _)  Ref{} ty pos = do
    lit <- case op of Equal    -> return (BoolLit False)
                      NotEqual -> return (BoolLit True)
                      _        -> stop state "evaluateBinOp: unsupported operator"
    return $ Lit (lit pos) ty pos

evaluateBinOp state op (Lit (NullLit _) _ _) (Lit (NullLit _) _ _) ty pos = do
    lit <- case op of Equal    -> return (BoolLit True)
                      NotEqual -> return (BoolLit False)
                      _        -> stop state "evaluateBinOp: unsupported operator"
    return $ Lit (lit pos) ty pos

evaluateBinOp state op expressionA@(SymbolicRef a _ _) expressionB@(SymbolicRef b _ _) ty pos
    | a == b 
        = case op of
                Equal    -> return $ Lit (BoolLit True pos) ty pos
                NotEqual -> return $ Lit (BoolLit False pos) ty pos
                _        -> stop state "evaluateBinOp: unsupported operator"
    | otherwise
        = return $ BinOp op expressionA expressionB ty pos

-- Default Evaluation
evaluateBinOp _ op expressionA expressionB ty pos =
    return $ BinOp op expressionA expressionB ty pos

evaluateUnOp :: UnOp -> Expression -> RuntimeType -> Position -> Engine r Expression
-- Negative Evaluation
evaluateUnOp Negative (Lit (IntLit value litPos) _ _) ty pos = return $ Lit (IntLit (-value) litPos) ty pos
evaluateUnOp Negative expression                      ty pos = return $ UnOp Negative expression ty pos
-- Negate Evaluation
evaluateUnOp Negate (Lit (BoolLit value litPos) _ _) ty pos = return $ Lit (BoolLit (not value) litPos) ty pos
evaluateUnOp Negate expression                       ty pos = return $ UnOp Negate expression ty pos
