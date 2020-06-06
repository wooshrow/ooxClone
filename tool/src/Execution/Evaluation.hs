{-# LANGUAGE LiberalTypeSynonyms #-}

module Execution.Evaluation(
      evaluateAsInt
    , evaluateAsBool
    , evaluate
) where

import Polysemy
import Polysemy.State
import Polysemy.Reader
import Polysemy.Error
import Polysemy.Cache              
import Polysemy.LocalState
import Data.Configuration
import Data.Statistics
import Verification.Result
import Analysis.CFA.CFG
import Analysis.SymbolTable
import Execution.Concurrency.Thread
import Execution.ExecutionState
import Execution.Memory.Heap
import Data.Positioned
import Language.Syntax
import Language.Syntax.Fold
import Language.Syntax.DSL

evaluateAsInt :: Thread -> Expression -> Engine r (EvaluationResult Int)
evaluateAsInt thread expression = do
    result <- evaluate thread expression
    case result of
        (Lit (IntLit value _) _ _) -> return $ Right value
        _                          -> return $ Left result

evaluateAsBool :: Thread -> Expression -> Engine r (EvaluationResult Bool)
evaluateAsBool thread expression = do
    result <- evaluate thread expression
    case result of
        (Lit (BoolLit value _) _ _) -> return $ Right value
        _                           -> return $ Left result

evaluate :: Thread -> Expression -> Engine r Expression
evaluate thread0 expression = foldExpression algebra expression thread0
    where
        algebra :: Members [ Reader (Configuration, ControlFlowGraph, SymbolTable)
                           , Error VerificationResult
                           , Cache Expression
                           , LocalState ExecutionState
                           , State Statistics
                           , Embed IO] r => ExpressionAlgebra (Thread -> Sem r Expression)
        algebra = ExpressionAlgebra
            { fForall = evaluateQuantifier ands'
            
            , fExists = evaluateQuantifier ors' 
            
            , fBinOp = \ binOp lhs0 rhs0 ty pos thread -> do
                lhs1 <- lhs0 thread
                rhs1 <- rhs0 thread
                evaluateBinOp binOp lhs1 rhs1 ty pos
                
            , fUnOp = \ unOp value0 ty pos thread -> do
                value1 <- value0 thread
                evaluateUnOp unOp value1 ty pos
                
            , fVar = \ var _ _ thread -> do
                value <- readVar thread var
                evaluate thread value
                
            , fSymVar = \ var ty pos _ -> 
                return $ SymbolicVar var ty pos
                
            , fLit = \ lit ty pos _ -> 
                return $ Lit lit ty pos
                
            , fSizeOf = \ var _ _ thread -> do
                ref <- readVar thread var
                processRef ref
                    (fmap (lit' . intLit') . arraySize)
                    (const (throw (InternalError "evaluate: SizeOf of symbolic reference")))
                    infeasible
                    
            , fRef = \ ref ty pos _ -> 
                return $ Ref ref ty pos
                
            , fSymRef = \ ref ty pos _ -> do
                let expr = SymbolicRef ref ty pos
                initializeSymbolicRef expr
                return expr
                
            , fCond = \ guard0 true0 false0 ty pos thread -> do
                guard1 <- guard0 thread
                case guard1 of
                    (Lit (BoolLit True  _) _ _) -> true0 thread
                    (Lit (BoolLit False _) _ _) -> false0 thread
                    _                           -> do
                        true1  <- true0 thread
                        false1 <- false0 thread
                        return $ Conditional guard1 true1 false1 ty pos }

evaluateQuantifier :: ([Expression] -> Expression) -> Identifier -> Identifier -> Identifier -> (Thread -> Engine r Expression) -> RuntimeType -> Position -> Thread -> Engine r Expression
evaluateQuantifier quantifier element range domain formula0 _ _ thread0 = do
    ref <- readVar thread0 domain
    processRef ref
        (\ concRef -> do
            structure <- dereference concRef
            let (ArrayValue values) = structure
            formulas <- mapM (\ (value, index) -> do
                state    <- getLocal
                thread1  <- writeVar thread0 element value
                thread2  <- writeVar thread1 range index
                formula1 <- formula0 thread2
                putLocal state
                return formula1
                ) ((zip values . map (lit' . intLit')) [0..])
            evaluate thread0 (quantifier formulas))
        (const (throw (InternalError "evaluateQuantifierAsBool: symbolic reference")))
        infeasible

evaluateBinOp :: BinOp -> Expression -> Expression ->  RuntimeType -> Position -> Engine r Expression
-- Boolean Evaluation
evaluateBinOp op (Lit (BoolLit a _) _ _) (Lit (BoolLit b _) _ _) ty pos = do
    lit <- case op of Implies  -> return (BoolLit (not a || b)); And   -> return (BoolLit (a && b))
                      Or       -> return (BoolLit (a || b))    ; Equal -> return (BoolLit (a == b))
                      NotEqual -> return (BoolLit (a /= b))    
                      _        -> throw (InternalError "evaluateBinOp: unsupported operator")
    return $ Lit (lit pos) ty pos

evaluateBinOp op expressionA@(Lit (BoolLit a _) _ _) expressionB ty pos =
    return $ case op of 
        Implies -> if a then expressionB else Lit (BoolLit True pos) ty pos
        And     -> if a then expressionB else Lit (BoolLit False pos) ty pos
        Or      -> if a then Lit (BoolLit True pos) ty pos else expressionB
        _       -> BinOp op expressionA expressionB ty pos
        
evaluateBinOp op expressionA expressionB@(Lit (BoolLit b _) _ _) ty pos =
    return $ case op of 
        Implies -> if b then Lit (BoolLit True pos) ty pos else UnOp Negate expressionA ty pos
        And     -> if b then expressionA else Lit (BoolLit False pos) ty pos
        Or      -> if b then Lit (BoolLit True pos) ty pos else expressionA
        _       -> BinOp op expressionA expressionB ty pos

-- Integer Evaluation
evaluateBinOp Divide _ (Lit (IntLit 0 _) _ _) _ _ = infeasible
evaluateBinOp Modulo _ (Lit (IntLit 0 _) _ _) _ _ = infeasible
evaluateBinOp op (Lit (IntLit a _) _ _) (Lit (IntLit b _) _ _) ty pos = do
    lit <- case op of Equal       -> return (BoolLit (a == b))  ; NotEqual         -> return (BoolLit (a /= b))
                      LessThan    -> return (BoolLit (a < b))   ; LessThanEqual    -> return (BoolLit (a <= b))
                      GreaterThan -> return (BoolLit (a > b))   ; GreaterThanEqual -> return (BoolLit (a >= b))
                      Plus        -> return (IntLit  (a + b))   ; Minus            -> return (IntLit  (a - b))
                      Multiply    -> return (IntLit  (a * b))   ; Divide           -> return (IntLit (a `div` b))
                      Modulo      -> return (IntLit (a `mod` b))
                      _           -> throw (InternalError "evaluateBinOp: unsupported operator")
    return $ Lit (lit pos) ty pos

-- Reference Evaluation
evaluateBinOp op (Ref a _ _) (Ref b _ _) ty pos = do
    lit <- case op of Equal    -> return (BoolLit (a == b)) 
                      NotEqual -> return (BoolLit (a /= b))
                      _        -> throw (InternalError "evaluateBinOp: unsupported operator")
    return $ Lit (lit pos) ty pos
evaluateBinOp op Ref{} (Lit (NullLit _) _ _) ty pos = do
    lit <- case op of Equal    -> return (BoolLit False)
                      NotEqual -> return (BoolLit True)
                      _        -> throw (InternalError "evaluateBinOp: unsupported operator")
    return $ Lit (lit pos) ty pos
evaluateBinOp op (Lit (NullLit _) _ _)  Ref{} ty pos = do
    lit <- case op of Equal    -> return (BoolLit False)
                      NotEqual -> return (BoolLit True)
                      _        -> throw (InternalError "evaluateBinOp: unsupported operator")
    return $ Lit (lit pos) ty pos
evaluateBinOp op (Lit (NullLit _) _ _) (Lit (NullLit _) _ _) ty pos = do
    lit <- case op of Equal    -> return (BoolLit True)
                      NotEqual -> return (BoolLit False)
                      _        -> throw (InternalError "evaluateBinOp: unsupported operator")
    return $ Lit (lit pos) ty pos
evaluateBinOp op expressionA@(SymbolicRef a _ _) expressionB@(SymbolicRef b _ _) ty pos
    | a == b 
        = case op of
                Equal    -> return $ Lit (BoolLit True pos) ty pos
                NotEqual -> return $ Lit (BoolLit False pos) ty pos
                _        -> throw (InternalError "evaluateBinOp: unsupported operator")
    | otherwise
        = return $ BinOp op expressionA expressionB ty pos

-- Default Evaluation
evaluateBinOp op expressionA expressionB ty pos =
    return $ BinOp op expressionA expressionB ty pos

evaluateUnOp :: UnOp -> Expression -> RuntimeType -> Position -> Engine r Expression
-- Negative Evaluation
evaluateUnOp Negative (Lit (IntLit value litPos) _ _) ty pos = return $ Lit (IntLit (-value) litPos) ty pos
evaluateUnOp Negative expression                      ty pos = return $ UnOp Negative expression ty pos
-- Negate Evaluation
evaluateUnOp Negate (Lit (BoolLit value litPos) _ _) ty pos = return $ Lit (BoolLit (not value) litPos) ty pos
evaluateUnOp Negate expression                       ty pos = return $ UnOp Negate expression ty pos
