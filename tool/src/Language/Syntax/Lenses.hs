module Language.Syntax.Lenses where

import Control.Lens
import Language.Syntax

-- $(makeFieldsNoPrefix ''Label)
$(makeFieldsNoPrefix ''Identifier)
$(makeFieldsNoPrefix ''RuntimeType)
$(makePrisms ''RuntimeType)
$(makeFieldsNoPrefix ''NonVoidType)
$(makeFieldsNoPrefix ''Type)
$(makeFieldsNoPrefix ''Lit)
$(makeFieldsNoPrefix ''Expression)
$(makeFieldsNoPrefix ''Invocation)
$(makeFieldsNoPrefix ''Lhs)
$(makePrisms ''Rhs)
$(makeFieldsNoPrefix ''Rhs)
$(makePrisms ''Statement)
$(makeFieldsNoPrefix ''Statement)
$(makeFieldsNoPrefix ''Specification)
$(makeFieldsNoPrefix ''Parameter)
$(makeFieldsNoPrefix ''DeclarationMember)
$(makePrisms ''DeclarationMember)
$(makeFieldsNoPrefix ''Declaration)
$(makeFieldsNoPrefix ''CompilationUnit)
    