{-# OPTIONS_GHC -fno-warn-orphans #-}

module Syntax.Pretty where

import Prelude            hiding ((<>))
import Text.Pretty
import Syntax.Syntax

--------------------------------------------------------------------------------
-- Top-level pretty printing
--------------------------------------------------------------------------------

instance Pretty CompilationUnit where
    pretty CompilationUnit{..}
        = pretty _members
    
instance Pretty [Declaration] where
    pretty = vsep

instance Pretty Declaration where
    pretty Class{..}
        = text "class" <+> pretty _name 
        $+$ lbrace $+$ tab (pretty _members) $+$ rbrace

instance Pretty [DeclarationMember] where
    pretty = vsep

instance Pretty DeclarationMember where
    pretty Constructor{..}
        = pretty _name <> pretty _params 
        $+$ tab (pretty _specification)
        $+$ lbrace $+$ tab (pretty _body) $+$ rbrace

    pretty Method{..}
        = pIsStatic <+> pretty _returnTy <+> pretty _name <> pretty _params
        $+$ tab (pretty _specification)
        $+$ lbrace $+$ tab (pretty _body) $+$ rbrace
        where
            pIsStatic | _isStatic = text "static"
                      | otherwise = empty

    pretty Field{..}
        = pretty _ty <+> pretty _name <> semi
        
instance Pretty [Parameter] where
    pretty = parens . commas

instance Pretty Parameter where
    pretty Parameter{..}
        = pretty _ty <+> pretty _name

instance Pretty Specification where
    pretty Specification{..}
        =   maybe empty ( \ e -> text "requires"    <> parens (pretty e)) _requires
        $+$ maybe empty ( \ e -> text "ensures"     <> parens (pretty e)) _ensures
        $+$ maybe empty ( \ e -> text "exceptional" <> parens (pretty e)) _exceptional

--------------------------------------------------------------------------------
-- Statement pretty printing
--------------------------------------------------------------------------------

instance Pretty [Statement] where
    pretty = vsep

instance Pretty Statement where
    pretty Declare{..}
        = pretty _ty <+> pretty _var <> semi

    pretty Assign{..}
        = pretty _lhs <+> text ":=" <+> pretty _rhs <> semi

    pretty Call{..}
        = pretty _invocation <> semi
    
    pretty Skip{}
        = semi

    pretty Assert{..}
        = text "assert" <+> pretty _assertion <> semi

    pretty Assume{..}
        = text "assume" <+> pretty _assumption <> semi

    pretty While{..}
        = text "while" <> parens (pretty _guard) $+$ tabWhenInline _body

    pretty Ite{..}
        = text "if" <> parens (pretty _guard) 
        $+$ tabWhenInline _trueBody $+$ text "else" $+$ tabWhenInline _falseBody

    pretty Continue{..}
        = text "continue" <> semi
        
    pretty Break{..}
        = text "break" <> semi

    pretty Return{..}
        = text "return" <> maybe semi (\ e -> space <> pretty e <> semi) _expression
    
    pretty Throw{..}
        = text "throw" <+> doubleQuotes (text _message) <> semi

    pretty Try{..}
        =   text "try"   $+$ lbrace $+$ tab (pretty _tryBody)   $+$ rbrace
        $+$ text "catch" $+$ lbrace $+$ tab (pretty _catchBody) $+$ rbrace

    pretty Block{..}
        = lbrace $+$ tab (pretty _body) $+$ rbrace

    pretty Lock{..}
        = text "lock" <> parens (pretty _var) <> semi

    pretty Unlock{..} 
        = text "unlock" <> parens (pretty _var) <> semi

    pretty Fork{..}
        = text "fork" <+> pretty _invocation <> semi
        
    pretty Join{}
        = text "join" <> semi

    pretty Seq{..}
        = pretty _stat1 $+$ pretty _stat2
        
instance Pretty Invocation where
    pretty InvokeMethod{..}
        = pretty _lhs <> dot <> pretty _rhs <> parens (commas _arguments)
    pretty InvokeConstructor{..}
        = text "new" <+> pretty _className <> parens (commas _arguments)

instance Pretty Lhs where
    pretty LhsVar{..}   = pretty _var
    pretty LhsField{..} = pretty _var <> dot <> pretty _field
    pretty LhsElem{..}  = pretty _var <> brackets (pretty _index)

instance Pretty Rhs where
    pretty RhsExpression{..}
        = pretty _value
    pretty RhsCall{..}
        = pretty _invocation
    pretty RhsField{..}
        = pretty _var <> dot <> pretty _field
    pretty RhsElem{..}
        = pretty _var <> brackets (pretty _index)
    pretty RhsArray{..}
        = text "new" <+> pretty _arrayTy <> hcat (map (brackets . pretty) _sizes)

tabWhenInline :: Statement -> Doc
tabWhenInline s@Block{} = pretty s
tabWhenInline s         = tab (pretty s)

--------------------------------------------------------------------------------
-- Expression pretty printing
--------------------------------------------------------------------------------

instance Pretty Expression where
    pretty Forall{..}
        = text "forall" <+> pretty _elem <> comma <> pretty _range <+> colon <+> pretty _domain <+> colon <+> pretty _formula
    pretty Exists{..}
        = text "exists" <+> pretty _elem <> comma <> pretty _range <+> colon <+> pretty _domain <+> colon <+> pretty _formula
    pretty BinOp{..}
        = pretty _lhs <+> pretty _binOp <+> pretty _rhs
    pretty UnOp{..}
        = pretty _unOp <> parens (pretty _value)
    pretty Var{..}
        = pretty _var
    pretty SymbolicVar{..}
        = pretty _var
    pretty Lit{..}
        = pretty _lit
    pretty SizeOf{..}
        = text "#" <> pretty _var
    pretty Ref{..}
        = text "ref" <> parens (pretty _ref)
    pretty SymbolicRef{..}
        = pretty _var
    pretty IteE{..}
        = text "ite" <> parens (pretty _guard <> comma <> pretty _true <> comma <> pretty _false)

instance Pretty BinOp where
    pretty op = text $ case op of
        Implies       -> "==>"; And         -> "&&"; Or               -> "||"
        Equal         -> "==" ; NotEqual    -> "!="; LessThan         -> "<"
        LessThanEqual -> "<=" ; GreaterThan -> ">" ; GreaterThanEqual -> ">="
        Plus          -> "+"  ; Minus       -> "-" ; Multiply         -> "*"
        Divide        -> "/"  ; Modulo      -> "%"

instance Pretty UnOp where
    pretty Negative = text "-"
    pretty Negate   = text "!"

instance Pretty Lit where
    pretty BoolLit{..}   = text $ if _boolValue then "true" else "false"
    pretty UIntLit{..}   = pretty _uintValue
    pretty IntLit{..}    = pretty _intValue
    pretty FloatLit{..}  = pretty _floatValue
    pretty StringLit{..} = (doubleQuotes . text) _stringValue
    pretty CharLit{..}   = (quotes . text) _charValue
    pretty NullLit{}     = text "null"

--------------------------------------------------------------------------------
-- Type pretty printing
--------------------------------------------------------------------------------

instance Pretty Type where
    pretty Type{..}
        = case _ty of
            Just ty' -> pretty ty'
            Nothing  -> text "void"

instance Pretty NonVoidType where
    pretty UIntType{}        = text "uint"
    pretty IntType{}         = text "int"
    pretty FloatType{}       = text "float"
    pretty BoolType{}        = text "bool"
    pretty StringType{}      = text "string"
    pretty CharType{}        = text "char"
    pretty ReferenceType{..} = pretty _ty 
    pretty ArrayType{..}     = pretty _innerTy <> text "[]"

instance Pretty RuntimeType where
    pretty UnknownRuntimeType       = text "?"
    pretty VoidRuntimeType          = text "void"
    pretty UIntRuntimeType          = text "uint"
    pretty IntRuntimeType           = text "int"
    pretty FloatRuntimeType         = text "float"
    pretty BoolRuntimeType          = text "bool"
    pretty StringRuntimeType        = text "string"
    pretty CharRuntimeType          = text "char"
    pretty ReferenceRuntimeType{..} = pretty _ty
    pretty ArrayRuntimeType{..}     = pretty _innerTy <> text "[]"
    pretty ANYRuntimeType           = text "any type"
    pretty NUMRuntimeType           = text "a numeric type"
    pretty REFRuntimeType           = text "a reference type"
    pretty ARRAYRuntimeType         = text "an array type"

--------------------------------------------------------------------------------
-- Auxiliary pretty printing
--------------------------------------------------------------------------------

instance Pretty Identifier where
    pretty Identifier{..} = text _name
