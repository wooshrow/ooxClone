module Syntax.DSL where

import Syntax.Syntax
import Analysis.Type.Typeable
import Data.Positioned

--------------------------------------------------------------------------------
-- Method and Constructor DSL
--------------------------------------------------------------------------------

parameter' :: NonVoidType -> Identifier -> Parameter
parameter' ty name = Parameter ty name unknownPos

--------------------------------------------------------------------------------
-- Statement DSL
--------------------------------------------------------------------------------

assign' :: Lhs -> Rhs -> Statement
assign' lhs rhs = Assign lhs rhs unknownLabel unknownPos

rhsExpr' :: Expression -> Rhs
rhsExpr' expr = RhsExpression expr (typeOf expr) (getPos expr)

unknownLabel :: Label
unknownLabel = error "non-assigned label"

--------------------------------------------------------------------------------
-- Expression DSL
--------------------------------------------------------------------------------

this' :: Identifier
this' = Identifier "this" unknownPos

retval' :: Identifier
retval' = Identifier "retval" unknownPos

var' :: Identifier -> RuntimeType -> Expression
var' var ty = Var var ty (getPos var)

symbolicVar' :: Identifier -> RuntimeType -> Expression
symbolicVar' var ty = SymbolicVar var ty unknownPos

lit' :: Lit -> Expression
lit' lit = Lit lit (typeOf lit) (getPos lit)

sizeof' :: Identifier -> Expression
sizeof' var = SizeOf var IntRuntimeType (getPos var)

ref' :: Reference -> RuntimeType -> Expression
ref' ref ty = Ref ref ty unknownPos

symbolicRef' :: Identifier -> RuntimeType -> Expression
symbolicRef' var ty = SymbolicRef var ty unknownPos

iteE' :: Expression -> Expression -> Expression -> Expression
iteE' guard true false = IteE guard true false BoolRuntimeType unknownPos

--------------------------------------------------------------------------------
-- Binary Operators

implies' :: Expression -> Expression -> Expression
implies' lhs rhs = BinOp Implies lhs rhs BoolRuntimeType (getPos lhs)

ands' :: Foldable f => f Expression -> Expression
ands' = foldr1 and'

and' :: Expression -> Expression -> Expression
and' lhs rhs = BinOp And lhs rhs BoolRuntimeType (getPos lhs)

ors' :: Foldable f => f Expression -> Expression
ors' = foldr1 or'

or' :: Expression -> Expression -> Expression
or' lhs rhs = BinOp Or lhs rhs BoolRuntimeType (getPos lhs)

equal' :: Expression -> Expression -> Expression
equal' lhs rhs = BinOp Equal lhs rhs BoolRuntimeType (getPos lhs)

lessthan' :: Expression -> Expression -> Expression
lessthan' lhs rhs = BinOp LessThan lhs rhs BoolRuntimeType (getPos lhs)

lessthanequal' :: Expression -> Expression -> Expression
lessthanequal' lhs rhs = BinOp LessThanEqual lhs rhs BoolRuntimeType (getPos lhs)

greaterthanequal' :: Expression -> Expression -> Expression
greaterthanequal' lhs rhs = BinOp GreaterThanEqual lhs rhs BoolRuntimeType (getPos lhs)

--------------------------------------------------------------------------------
-- Unary Operators

neg' :: Expression -> Expression
neg' expression = UnOp Negate expression BoolRuntimeType (getPos expression)

--------------------------------------------------------------------------------
-- Literals

uIntLit' :: Int -> Lit
uIntLit' value
    | value < 0 = error "uIntLit': value < 0"
    | otherwise = UIntLit value unknownPos

intLit' :: Int -> Lit
intLit' value = IntLit value unknownPos

floatLit' :: Float -> Lit
floatLit' value = FloatLit value unknownPos

boolLit' :: Bool -> Lit
boolLit' value = BoolLit value unknownPos

charLit' :: String -> Lit
charLit' value 
    | [_] <- value = CharLit value unknownPos
    | otherwise    = error "charLit': value not a single character"

nullLit' :: Lit
nullLit' = NullLit unknownPos

--------------------------------------------------------------------------------
-- Type DSL
--------------------------------------------------------------------------------

refType' :: Identifier -> NonVoidType
refType' ty = ReferenceType ty unknownPos