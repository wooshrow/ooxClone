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
import Data.Configuration
import Data.Statistics
import Verification.Result
import Analysis.CFA.CFG
import Analysis.SymbolTable
import Execution.Concurrency.Thread
import Execution.ExecutionState
import Execution.Memory.Heap
import Data.Positioned
import Syntax.Syntax
import Syntax.Fold
import Syntax.DSL

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
                    (error "evaluate: SizeOf of symbolic reference")
                    infeasible
                    
            , fRef = \ ref ty pos _ -> 
                return $ Ref ref ty pos
                
            , fSymRef = \ ref ty pos _ -> do
                let expr = SymbolicRef ref ty pos
                initializeSymbolicRef expr
                return expr
                
            , fIte = \ guard0 true0 false0 ty pos thread -> do
                guard1 <- guard0 thread
                case guard1 of
                    (Lit (BoolLit True  _) _ _) -> true0 thread
                    (Lit (BoolLit False _) _ _) -> false0 thread
                    _                           -> do
                        true1  <- true0 thread
                        false1 <- false0 thread
                        return $ IteE guard1 true1 false1 ty pos }

evaluateQuantifier :: ([Expression] -> Expression) -> Identifier -> Identifier -> Identifier -> (Thread -> Engine r Expression) -> RuntimeType -> Position -> Thread -> Engine r Expression
evaluateQuantifier quantifier elem range domain formula0 _ _ thread0 = do
    ref <- readVar thread0 domain
    processRef ref
        (\ concRef -> do
            structure <- dereference concRef
            let (ArrayValue values) = structure
            formulas <- mapM (\ (value, index) -> do
                state    <- getLocal
                thread1  <- writeVar thread0 elem value
                thread2  <- writeVar thread1 range index
                formula1 <- formula0 thread2
                putLocal state
                return formula1
                ) ((zip values . map (lit' . intLit')) [0..])
            evaluate thread0 (quantifier formulas))
        (error "evaluateQuantifierAsBool: symbolic reference")
        infeasible

evaluateBinOp :: BinOp -> Expression -> Expression ->  RuntimeType -> Position -> Engine r Expression
-- Boolean Evaluation
evaluateBinOp op (Lit (BoolLit a _) _ _) (Lit (BoolLit b _) _ _) ty pos = do
    let lit = case op of Implies  -> BoolLit (not a || b); And   -> BoolLit (a && b)
                         Or       -> BoolLit (a || b)    ; Equal -> BoolLit (a == b)
                         NotEqual -> BoolLit (a /= b)    
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
    let lit = case op of Equal       -> BoolLit (a == b)   ; NotEqual         -> BoolLit (a /= b)   
                         LessThan    -> BoolLit (a < b)    ; LessThanEqual    -> BoolLit (a <= b)   
                         GreaterThan -> BoolLit (a > b)    ; GreaterThanEqual -> BoolLit (a >= b)   
                         Plus        -> IntLit  (a + b)    ; Minus            -> IntLit  (a - b)    
                         Multiply    -> IntLit  (a * b)    ; Divide           -> IntLit (a `div` b)
                         Modulo      -> IntLit (a `mod` b)
    return $ Lit (lit pos) ty pos

-- Reference Evaluation
evaluateBinOp op (Ref a _ _) (Ref b _ _) ty pos = do
    let lit = case op of Equal -> BoolLit (a == b); NotEqual -> BoolLit (a /= b)
    return $ Lit (lit pos) ty pos
evaluateBinOp op Ref{} (Lit (NullLit _) _ _) ty pos = do
    let lit = case op of Equal -> BoolLit False; NotEqual -> BoolLit True
    return $ Lit (lit pos) ty pos
evaluateBinOp op (Lit (NullLit _) _ _)  Ref{} ty pos = do
    let lit = case op of Equal -> BoolLit False; NotEqual -> BoolLit True
    return $ Lit (lit pos) ty pos
evaluateBinOp op (Lit (NullLit _) _ _) (Lit (NullLit _) _ _) ty pos = do
    let lit = case op of Equal -> BoolLit True; NotEqual -> BoolLit False
    return $ Lit (lit pos) ty pos
evaluateBinOp op expressionA@(SymbolicRef a _ _) expressionB@(SymbolicRef b _ _) ty pos
    | a == b 
        = return $ Lit (BoolLit (case op of Equal -> True; NotEqual -> False) pos) ty pos
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
