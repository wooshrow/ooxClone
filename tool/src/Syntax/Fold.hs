module Syntax.Fold(
      StatementAlgebra
    , foldStatement
    , ExpressionAlgebra(..)
    , monoidExpressionAlgebra
    , monoidMExpressionAlgebra
    , identityExpressionAlgebra
    , foldExpression
) where

import Data.Positioned
import Syntax.Syntax

type StatementAlgebra r
    = ( NonVoidType -> Identifier -> Label -> SourcePos -> r         -- Declaration
      , Lhs -> Rhs -> Label -> SourcePos -> r                        -- Assignment
      , Invocation -> Label -> SourcePos -> r                        -- Call
      , Label -> SourcePos -> r                                      -- Skip
      , Expression -> Label -> SourcePos -> r                        -- Assert
      , Expression -> Label -> SourcePos -> r                        -- Assume
      , Expression -> r -> Label -> SourcePos -> r                   -- While
      , Expression -> r -> r -> Label -> SourcePos -> r              -- Ite
      , Label -> SourcePos -> r                                      -- Continue
      , Label -> SourcePos -> r                                      -- Break
      , Maybe Expression -> Label -> SourcePos -> r                  -- Return
      , String -> Label -> SourcePos -> r                            -- Throw
      , r -> r -> Label -> Label -> Label -> Label -> SourcePos -> r -- Try
      , r -> Label -> SourcePos -> r                                 -- Block
      , Identifier -> Label -> SourcePos -> r                        -- Lock
      , Identifier -> Label -> SourcePos -> r                        -- Unlock
      , Invocation -> Label -> SourcePos -> r                        -- Fork
      , Label -> SourcePos -> r                                      -- Join
      , r -> r -> Label -> SourcePos -> r)                           -- Seq

foldStatement :: StatementAlgebra r -> Statement -> r
foldStatement (fDeclaration, fAssignment, fCall, fSkip, fAssert, fAssume, fWhile, fIte, fContinue, fBreak, fReturn, fThrow, fTry, fBlock, fLock, fUnlock, fFork, fJoin, fSeq)
    = fold
    where
        fold Declare{..}  = fDeclaration _ty _var _label _info
        fold Assign{..}   = fAssignment _lhs _rhs _label _info
        fold Call{..}     = fCall _invocation _label _info
        fold Skip{..}     = fSkip _label _info
        fold Assert{..}   = fAssert _assertion _label _info
        fold Assume{..}   = fAssume _assumption _label _info
        fold While{..}    = fWhile _guard (fold _body) _label _info
        fold Ite{..}      = fIte _guard (fold _trueBody) (fold _falseBody) _label _info
        fold Continue{..} = fContinue _label _info
        fold Break{..}    = fBreak _label _info
        fold Return{..}   = fReturn _expression _label _info
        fold Throw{..}    = fThrow _message _label _info
        fold Try{..}      = fTry (fold _tryBody) (fold _catchBody) _label _label2 _label3 _label4 _info
        fold Block{..}    = fBlock (fold _body) _label _info
        fold Lock{..}     = fLock _var _label _info
        fold Unlock{..}   = fUnlock _var _label _info
        fold Fork{..}     = fFork _invocation _label _info
        fold Join{..}     = fJoin _label _info
        fold Seq{..}      = fSeq (fold _stat1) (fold _stat2) _label _info
        
data ExpressionAlgebra r 
    = ExpressionAlgebra 
    { fForall :: Identifier -> Identifier -> Identifier -> r -> RuntimeType -> SourcePos -> r
    , fExists :: Identifier -> Identifier -> Identifier -> r -> RuntimeType -> SourcePos -> r
    , fBinOp  :: BinOp -> r -> r -> RuntimeType -> SourcePos -> r
    , fUnOp   :: UnOp -> r -> RuntimeType -> SourcePos -> r 
    , fVar    :: Identifier -> RuntimeType -> SourcePos -> r
    , fSymVar :: Identifier -> RuntimeType -> SourcePos -> r
    , fLit    :: Lit -> RuntimeType -> SourcePos -> r
    , fSizeOf :: Identifier -> RuntimeType -> SourcePos -> r
    , fRef    :: Int -> RuntimeType -> SourcePos -> r
    , fSymRef :: Identifier -> RuntimeType -> SourcePos -> r
    , fIte    :: r -> r -> r -> RuntimeType -> SourcePos -> r }
    
monoidExpressionAlgebra :: Monoid r => ExpressionAlgebra r
monoidExpressionAlgebra = ExpressionAlgebra
    { fForall = \ _ _ _ formula _ _    -> formula
    , fExists = \ _ _ _ formula _ _    -> formula            
    , fBinOp  = \ _ lhs rhs _ _        -> lhs <> rhs
    , fUnOp   = \ _ value _ _          -> value
    , fVar    = \ _ _ _                -> mempty
    , fSymVar = \ _ _ _                -> mempty
    , fLit    = \ _ _ _                -> mempty
    , fSizeOf = \ var _ _              -> mempty
    , fRef    = \ _ _ _                -> mempty
    , fSymRef = \ _ _ _                -> mempty
    , fIte    = \ guard true false _ _ -> guard <> true <> false }

monoidMExpressionAlgebra :: (Monoid r, Monad m) => ExpressionAlgebra (m r)
monoidMExpressionAlgebra = ExpressionAlgebra
    { fForall = \ _ _ _ formula _ _    -> formula
    , fExists = \ _ _ _ formula _ _    -> formula            
    , fBinOp  = \ _ lhs rhs _ _        -> (<>) <$> lhs <*> rhs
    , fUnOp   = \ _ value _ _          -> value
    , fVar    = \ _ _ _                -> return mempty
    , fSymVar = \ _ _ _                -> return mempty
    , fLit    = \ _ _ _                -> return mempty
    , fSizeOf = \ var _ _              -> return mempty
    , fRef    = \ _ _ _                -> return mempty
    , fSymRef = \ _ _ _                -> return mempty
    , fIte    = \ guard true false _ _ -> (\ g t f -> g <> t <> f) <$> guard <*> true <*> false }

identityExpressionAlgebra :: ExpressionAlgebra Expression
identityExpressionAlgebra = ExpressionAlgebra
    { fForall = Forall
    , fExists = Exists          
    , fBinOp  = BinOp
    , fUnOp   = UnOp
    , fVar    = Var
    , fSymVar = SymbolicVar
    , fLit    = Lit
    , fSizeOf = SizeOf
    , fRef    = Ref
    , fSymRef = SymbolicRef
    , fIte    = IteE }

foldExpression :: ExpressionAlgebra r -> Expression -> r
foldExpression ExpressionAlgebra{..} = fold
    where 
        fold Forall{..}      = fForall _elem _range _domain (fold _formula) _ty _info
        fold Exists{..}      = fExists _elem _range _domain (fold _formula) _ty _info
        fold BinOp{..}       = fBinOp _binOp (fold _lhs) (fold _rhs) _ty _info
        fold UnOp{..}        = fUnOp _unOp (fold _value) _ty _info
        fold Var{..}         = fVar _var _ty _info
        fold SymbolicVar{..} = fSymVar _var _ty _info
        fold Lit{..}         = fLit _lit _ty _info
        fold SizeOf{..}      = fSizeOf _var _ty _info
        fold Ref{..}         = fRef _ref _ty _info
        fold SymbolicRef{..} = fSymRef _var _ty _info
        fold IteE{..}        = fIte (fold _guard) (fold _true) (fold _false) _ty _info

{-type ExpressionAlgebra r
    = ( Identifier -> Identifier -> Identifier -> r -> RuntimeType -> SourcePos -> r -- Forall
      , Identifier -> Identifier -> Identifier -> r -> RuntimeType -> SourcePos -> r -- Exists
      , BinOp -> r -> r -> RuntimeType -> SourcePos -> r                             -- BinOp
      , UnOp -> r -> RuntimeType -> SourcePos -> r                                   -- UnOp
      , Identifier -> RuntimeType -> SourcePos -> r                                  -- Var
      , Identifier -> RuntimeType -> SourcePos -> r                                  -- SymbolicVar
      , Lit -> RuntimeType -> SourcePos -> r                                         -- Lit
      , Identifier -> RuntimeType -> SourcePos -> r                                  -- SizeOf
      , Int -> RuntimeType -> SourcePos -> r                                         -- Ref
      , Identifier -> RuntimeType -> SourcePos -> r                                  -- SymbolicRef
      , r -> r -> r -> RuntimeType -> SourcePos -> r)                                -- Ite


foldExpression :: ExpressionAlgebra r -> Expression -> r
foldExpression (fForall, fExists, fBinOp, fUnOp, fVar, fSymbolicVar, fLit, fSizeOf, fRef, fSymbolicRef, fIte)
    = fold
    where
        fold Forall{..}      = fForall _elem _range _domain (fold _formula) _ty _info
        fold Exists{..}      = fExists _elem _range _domain (fold _formula) _ty _info
        fold BinOp{..}       = fBinOp _binOp (fold _lhs) (fold _rhs) _ty _info
        fold UnOp{..}        = fUnOp _unOp (fold _value) _ty _info
        fold Var{..}         = fVar _var _ty _info
        fold SymbolicVar{..} = fSymbolicVar _var _ty _info
        fold Lit{..}         = fLit _lit _ty _info
        fold SizeOf{..}      = fSizeOf _var _ty _info
        fold Ref{..}         = fRef _ref _ty _info
        fold SymbolicRef{..} = fSymbolicRef _var _ty _info
        fold IteE{..}        = fIte (fold _guard) (fold _true) (fold _false) _ty _info
-}