{-# LANGUAGE DeriveGeneric #-}

module Syntax.Syntax(
      CompilationUnit(..)
    , Declaration(..)
    , DeclarationMember(..)
    , Parameter(..)
    , Specification(..)
    , Statement(..)
    , Invocation(..)
    , Lhs(..)
    , Rhs(..)
    , Expression(..)
    , BinOp(..)
    , UnOp(..)
    , Lit(..)
    , Type(..)
    , NonVoidType(..)
    , RuntimeType(..)
    , Label
    , Identifier(..)
    , Reference
) where

import GHC.Generics    (Generic)
import Data.Positioned
import Data.Hashable

--------------------------------------------------------------------------------
-- Top-level definitions
--------------------------------------------------------------------------------

data CompilationUnit
    = CompilationUnit { _members :: [Declaration], _info :: SourcePos }
    deriving (Show)

data Declaration
    = Class { _name :: Identifier, _members :: [DeclarationMember], _info :: SourcePos }
    deriving (Show)

instance Eq Declaration where
    (Class _ _ posX) == (Class _ _ posY) = posX == posY
    
instance Ord Declaration where
    (Class _ _ posX) <= (Class _ _ posY) = posX <= posY

data DeclarationMember
    = Constructor { _name          :: Identifier    , _params :: [Parameter]
                  , _specification :: Specification , _body   :: Statement
                  , _labels        :: (Label, Label), _info   :: SourcePos }
    | Method { _isStatic      :: Bool          , _returnTy :: Type
             , _name          :: Identifier    , _params   :: [Parameter]
             , _specification :: Specification , _body     :: Statement
             , _labels        :: (Label, Label), _info     :: SourcePos }
    | Field  { _ty :: NonVoidType, _name :: Identifier, _info :: SourcePos }
    deriving (Show)

instance Eq DeclarationMember where
    a == b = _info (a :: DeclarationMember) == _info (b :: DeclarationMember)
        
instance Ord DeclarationMember where
    a <= b = _info (a :: DeclarationMember) <= _info (b :: DeclarationMember)

instance WithPos DeclarationMember where
    getPos = _info

data Parameter
    = Parameter { _ty :: NonVoidType, _name :: Identifier, _info :: SourcePos }
    deriving (Show)

data Specification
    = Specification { _requires    :: Maybe Expression
                    , _ensures     :: Maybe Expression
                    , _exceptional :: Maybe Expression
                    , _info        :: SourcePos }
    deriving (Show)

--------------------------------------------------------------------------------
-- Statement definitions
--------------------------------------------------------------------------------

data Statement
    = Declare { _ty :: NonVoidType, _var :: Identifier, _label :: Label, _info :: SourcePos }
    | Assign { _lhs :: Lhs, _rhs :: Rhs, _label :: Label, _info :: SourcePos }
    | Call { _invocation :: Invocation, _label :: Label, _info :: SourcePos }
    | Skip { _label :: Label, _info :: SourcePos }
    | Assert { _assertion :: Expression, _label :: Label, _info :: SourcePos }
    | Assume { _assumption :: Expression, _label :: Label, _info :: SourcePos }
    | While { _guard :: Expression, _body :: Statement, _label :: Label, _info :: SourcePos }
    | Ite { _guard :: Expression, _trueBody :: Statement, _falseBody :: Statement, _label :: Label, _info :: SourcePos }
    | Continue { _label :: Label, _info :: SourcePos }
    | Break { _label :: Label, _info :: SourcePos }
    | Return { _expression :: Maybe Expression, _label :: Label, _info :: SourcePos }
    | Throw { _message :: String, _label :: Label, _info :: SourcePos }
    | Try { _tryBody :: Statement, _catchBody :: Statement, _label :: Label, _label2 :: Label, _label3 :: Label, _label4 :: Label, _info :: SourcePos }
    | Block { _body :: Statement, _label :: Label, _info :: SourcePos }
    | Lock { _var :: Identifier, _label :: Label, _info :: SourcePos }
    | Unlock { _var :: Identifier, _label :: Label, _info :: SourcePos }
    | Fork { _invocation :: Invocation, _label :: Label, _info :: SourcePos }
    | Join { _label :: Label, _info :: SourcePos }
    | Seq { _stat1 :: Statement, _stat2 :: Statement, _label :: Label, _info :: SourcePos }
    deriving (Show)

instance Eq Statement where
    a == b = (_label :: Statement -> Label) a == (_label :: Statement -> Label) b

instance WithPos Statement where
    getPos = _info

data Invocation
    = InvokeMethod { _lhs :: Identifier, _rhs :: Identifier, _arguments :: [Expression], _resolved :: Maybe (Declaration, DeclarationMember), _label :: Label, _info :: SourcePos }
    | InvokeConstructor { _className :: Identifier, _arguments :: [Expression], _resolved :: Maybe (Declaration, DeclarationMember), _label :: Label, _info :: SourcePos }
    deriving (Show)
 
instance Eq Invocation where
    a == b = (_label :: Invocation -> Label) a == (_label :: Invocation -> Label) b

instance WithPos Invocation where
    getPos = _info

data Lhs
    = LhsVar   { _var :: Identifier, _ty :: RuntimeType, _info :: SourcePos }
    | LhsField { _var :: Identifier, _varTy :: RuntimeType, _field :: Identifier, _ty :: RuntimeType, _info :: SourcePos }
    | LhsElem  { _var :: Identifier, _index :: Expression, _ty :: RuntimeType, _info :: SourcePos }
    deriving (Eq, Show)

data Rhs
    = RhsExpression { _value :: Expression, _ty :: RuntimeType, _info :: SourcePos }
    | RhsField { _var :: Expression, _field :: Identifier, _ty :: RuntimeType, _info :: SourcePos }
    | RhsElem { _var :: Expression, _index :: Expression, _ty :: RuntimeType, _info :: SourcePos }
    | RhsCall { _invocation :: Invocation, _ty :: RuntimeType, _info :: SourcePos }
    | RhsArray { _arrayTy :: NonVoidType, _sizes :: [Expression], _ty :: RuntimeType, _info :: SourcePos }
    deriving (Eq, Show)

instance WithPos Rhs where
    getPos = _info

--------------------------------------------------------------------------------
-- Expression definitions
--------------------------------------------------------------------------------

type Reference = Int

data Expression
    = Forall      { _elem :: Identifier, _range :: Identifier, _domain :: Identifier, _formula :: Expression, _ty :: RuntimeType, _info :: SourcePos }
    | Exists      { _elem :: Identifier, _range :: Identifier, _domain :: Identifier, _formula :: Expression, _ty :: RuntimeType, _info :: SourcePos }
    | BinOp       { _binOp :: BinOp, _lhs :: Expression, _rhs :: Expression, _ty :: RuntimeType, _info :: SourcePos }
    | UnOp        { _unOp :: UnOp, _value :: Expression, _ty :: RuntimeType, _info :: SourcePos }
    | Var         { _var :: Identifier, _ty :: RuntimeType, _info :: SourcePos }
    | SymbolicVar { _var :: Identifier, _ty :: RuntimeType, _info :: SourcePos }
    | Lit         { _lit :: Lit, _ty :: RuntimeType, _info :: SourcePos }
    | SizeOf      { _var :: Identifier, _ty :: RuntimeType, _info :: SourcePos }
    | Ref         { _ref :: Reference, _ty :: RuntimeType, _info :: SourcePos }
    | SymbolicRef { _var :: Identifier, _ty :: RuntimeType, _info :: SourcePos }
    | IteE        { _guard :: Expression, _true :: Expression, _false :: Expression, _ty :: RuntimeType, _info :: SourcePos }
    deriving (Show)

instance Eq Expression where
    (Forall elemA rangeA domainA formulaA _ _) == (Forall elemB rangeB domainB formulaB _ _) 
        = elemA == elemB && rangeA == rangeB && domainA == domainB && formulaA == formulaB
    (Exists elemA rangeA domainA formulaA _ _) == (Forall elemB rangeB domainB formulaB _ _) 
        = elemA == elemB && rangeA == rangeB && domainA == domainB && formulaA == formulaB
    (BinOp opA lhsA rhsA _ _) == (BinOp opB lhsB rhsB _ _)
        = opA == opB && lhsA == lhsB && rhsA == rhsB
    (UnOp opA valueA _ _) == (UnOp opB valueB _ _)
        = opA == opB && valueA == valueB
    (Var varA _ _) == (Var varB _ _)
        = varA == varB
    (SymbolicVar varA _ _) == (SymbolicVar varB _ _)
        = varA == varB
    (Lit litA _ _) == (Lit litB _ _)
        = litA == litB    
    (SizeOf varA _ _) == (SizeOf varB _ _)
        = varA == varB
    (Ref refA _ _) == (Ref refB _ _)
        = refA == refB
    (SymbolicRef varA _ _) == (SymbolicRef varB _ _)
        = varA == varB
    (IteE guardA trueA falseA _ _) == (IteE guardB trueB falseB _ _)
        = guardA == guardB && trueA == trueB && falseA == falseB
    _ == _ = False

instance Ord Expression where
    (Forall elemA rangeA domainA formulaA _ _) <= (Forall elemB rangeB domainB formulaB _ _) 
        = elemA <= elemB && rangeA <= rangeB && domainA <= domainB && formulaA <= formulaB
    (Exists elemA rangeA domainA formulaA _ _) <= (Forall elemB rangeB domainB formulaB _ _) 
        = elemA <= elemB && rangeA <= rangeB && domainA <= domainB && formulaA <= formulaB
    (BinOp opA lhsA rhsA _ _) <= (BinOp opB lhsB rhsB _ _)
        = opA <= opB && lhsA <= lhsB && rhsA <= rhsB
    (UnOp opA valueA _ _) <= (UnOp opB valueB _ _)
        = opA <= opB && valueA <= valueB
    (Var varA _ _) <= (Var varB _ _)
        = varA <= varB
    (SymbolicVar varA _ _) <= (SymbolicVar varB _ _)
        = varA <= varB
    (Lit litA _ _) <= (Lit litB _ _)
        = litA <= litB    
    (SizeOf varA _ _) <= (SizeOf varB _ _)
        = varA <= varB
    (Ref refA _ _) <= (Ref refB _ _)
        = refA <= refB
    (SymbolicRef varA _ _) <= (SymbolicRef varB _ _)
        = varA <= varB
    (IteE guardA trueA falseA _ _) <= (IteE guardB trueB falseB _ _)
        = guardA <= guardB && trueA <= trueB && falseA <= falseB
    _ <= _ = False

instance Hashable Expression where
    hashWithSalt salt (Forall element range domain formula _ _)
        = salt `hashWithSalt` element `hashWithSalt` range `hashWithSalt` domain `hashWithSalt` formula `hashWithSalt` (1 :: Int)
    hashWithSalt salt (Exists element range domain formula _ _)
        = salt `hashWithSalt` element `hashWithSalt` range `hashWithSalt` domain `hashWithSalt` formula `hashWithSalt` (2 :: Int)
    hashWithSalt salt (BinOp binOp lhs rhs _ _)
        = salt `hashWithSalt` binOp `hashWithSalt` lhs `hashWithSalt` rhs `hashWithSalt` (3 :: Int)
    hashWithSalt salt (UnOp unOp value _ _)
        = salt `hashWithSalt` unOp `hashWithSalt` value `hashWithSalt` (4 :: Int)
    hashWithSalt salt (Var var _ _)
        = hashWithSalt salt var `hashWithSalt` (5 :: Int)
    hashWithSalt salt (SymbolicVar var _ _)
        = hashWithSalt salt var `hashWithSalt` (6 :: Int)
    hashWithSalt salt (Lit lit _ _)
        = hashWithSalt salt lit `hashWithSalt` (7 :: Int)
    hashWithSalt salt (SizeOf var _ _)
        = hashWithSalt salt var `hashWithSalt` (8 :: Int)
    hashWithSalt salt (Ref ref _ _)
        = hashWithSalt salt ref `hashWithSalt` (9 :: Int)
    hashWithSalt salt (SymbolicRef var _ _)
        = hashWithSalt salt var `hashWithSalt` (10 :: Int)
    hashWithSalt salt (IteE guard true false _ _)
        = salt `hashWithSalt` guard `hashWithSalt` true `hashWithSalt` false `hashWithSalt` (111 :: Int)

instance WithPos Expression where
    getPos = _info

data BinOp 
    = Implies  | And           | Or          | Equal            | NotEqual 
    | LessThan | LessThanEqual | GreaterThan | GreaterThanEqual | Plus 
    | Minus    | Multiply      | Divide      | Modulo
    deriving (Show, Eq, Ord, Generic)

instance Hashable BinOp

data UnOp
    = Negative | Negate
    deriving (Show, Eq, Ord, Generic)
    
instance Hashable UnOp

data Lit
    = BoolLit   { _boolValue   :: Bool     , _info :: SourcePos }
    | UIntLit   { _uintValue   :: Int      , _info :: SourcePos }
    | IntLit    { _intValue    :: Int      , _info :: SourcePos }
    | FloatLit  { _floatValue  :: Float    , _info :: SourcePos }
    | StringLit { _stringValue :: String   , _info :: SourcePos }
    | CharLit   { _charValue   :: String   , _info :: SourcePos }
    | NullLit   { _info        :: SourcePos                     }
    deriving (Show)

instance Eq Lit where
    (BoolLit a _)   == (BoolLit b _)   = a == b
    (UIntLit a _)   == (UIntLit b _)   = a == b
    (IntLit a _)    == (IntLit b _)    = a == b
    (FloatLit a _)  == (FloatLit b _)  = a == b
    (StringLit a _) == (StringLit b _) = a == b
    (CharLit a _)   == (CharLit b _)   = a == b
    (NullLit _)     == (NullLit _)     = True
    _               == _               = False

instance Ord Lit where
    (BoolLit a _)   <= (BoolLit b _)   = a <= b
    (UIntLit a _)   <= (UIntLit b _)   = a <= b
    (IntLit a _)    <= (IntLit b _)    = a <= b
    (FloatLit a _)  <= (FloatLit b _)  = a <= b
    (StringLit a _) <= (StringLit b _) = a <= b
    (CharLit a _)   <= (CharLit b _)   = a <= b
    (NullLit _)     <= (NullLit _)     = True
    _               <= _               = False

instance Hashable Lit where
    hashWithSalt salt (BoolLit value _)   = hashWithSalt salt value `hashWithSalt` (1 :: Int)
    hashWithSalt salt (UIntLit value _)   = hashWithSalt salt value `hashWithSalt` (2 :: Int)
    hashWithSalt salt (IntLit value _)    = hashWithSalt salt value `hashWithSalt` (3 :: Int)
    hashWithSalt salt (FloatLit value _)  = hashWithSalt salt value `hashWithSalt` (4 :: Int)
    hashWithSalt salt (StringLit value _) = hashWithSalt salt value `hashWithSalt` (5 :: Int)
    hashWithSalt salt (CharLit value _)   = hashWithSalt salt value `hashWithSalt` (6 :: Int)
    hashWithSalt salt (NullLit _)         = hashWithSalt salt (7 :: Int)

instance WithPos Lit where
    getPos = _info

--------------------------------------------------------------------------------
-- Type definitions
--------------------------------------------------------------------------------

data Type
    = Type { _ty :: Maybe NonVoidType, _info :: SourcePos }
    deriving (Show, Eq)

data NonVoidType
    = UIntType      { _info :: SourcePos }
    | IntType       { _info :: SourcePos }
    | FloatType     { _info :: SourcePos }
    | BoolType      { _info :: SourcePos }
    | StringType    { _info :: SourcePos }
    | CharType      { _info :: SourcePos }
    | ReferenceType { _ty :: Identifier, _info :: SourcePos }
    | ArrayType     { _innerTy :: NonVoidType, _info :: SourcePos }
    deriving (Show, Eq)

data RuntimeType
    = UnknownRuntimeType
    | VoidRuntimeType
    | UIntRuntimeType
    | IntRuntimeType
    | FloatRuntimeType
    | BoolRuntimeType
    | StringRuntimeType
    | CharRuntimeType
    | ReferenceRuntimeType { _ty :: Identifier }
    | ArrayRuntimeType { _innerTy :: RuntimeType }
    | ANYRuntimeType
    | NUMRuntimeType
    | REFRuntimeType
    | ARRAYRuntimeType
    deriving (Show, Eq, Ord)

--------------------------------------------------------------------------------
-- Auxiliary definitions
--------------------------------------------------------------------------------

data Identifier
    = Identifier { _name :: String, _info :: SourcePos }
    deriving (Show)

instance Eq Identifier where
    (Identifier x _) == (Identifier y _) = x == y

instance Ord Identifier where
    (Identifier x _) <= (Identifier y _) = x <= y

instance Hashable Identifier where
    hashWithSalt salt (Identifier name _)
        = hashWithSalt salt name

instance WithPos Identifier where
    getPos = _info

type Label = Int