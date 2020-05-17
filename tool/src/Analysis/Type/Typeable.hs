module Analysis.Type.Typeable where

import           Control.Lens
import           Data.Positioned
import           Text.Pretty
import           Syntax.Syntax
import           Syntax.Pretty()
import qualified Syntax.Lenses   as SL

class Typeable a where
    typeOf :: a -> RuntimeType
    isOfType :: Typeable b => a -> b -> Bool
    isOfType lhs rhs 
        = typeOf lhs `matches` typeOf rhs
        where
            -- Matching ANY types
            matches ANYRuntimeType _ = True
            matches _ ANYRuntimeType = True
            -- Matching NUM types
            matches NUMRuntimeType b = b `elem` numTypes
            matches a NUMRuntimeType = a `elem` numTypes
            -- Matching REF types
            matches REFRuntimeType REFRuntimeType           = True 
            matches REFRuntimeType (ReferenceRuntimeType _) = True 
            matches REFRuntimeType (ArrayRuntimeType _)     = True 
            matches REFRuntimeType StringRuntimeType        = True 
            matches REFRuntimeType ARRAYRuntimeType         = True 
            matches (ReferenceRuntimeType _) REFRuntimeType = True 
            matches (ArrayRuntimeType _)     REFRuntimeType = True 
            matches StringRuntimeType        REFRuntimeType = True 
            matches ARRAYRuntimeType         REFRuntimeType = True 
            -- Matching ARRAY types
            matches ARRAYRuntimeType (ArrayRuntimeType _) = True
            matches (ArrayRuntimeType _) ARRAYRuntimeType = True
            -- Matching concrete types
            matches a b = a == b

            numTypes = [ NUMRuntimeType , IntRuntimeType
                       , UIntRuntimeType, FloatRuntimeType ]

instance Typeable a => Typeable (Positioned a) where
    typeOf Positioned{value} = typeOf value

instance Typeable DeclarationMember where
    typeOf c@Constructor{} = ReferenceRuntimeType{ _ty = c ^?! SL.name }
    typeOf m@Method{}      = typeOf (m ^?! SL.returnTy)
    typeOf f@Field{}       = typeOf (f ^?! SL.ty)

instance Typeable Invocation where
    typeOf i 
        | Just resolved <- i ^. SL.resolved
            = typeOf (snd resolved)
        | otherwise
            = error $ "typeOf unresolved method: '" ++ toString i ++ "'"

instance Typeable Lhs where
    typeOf lhs = lhs ^?! SL.ty
    
instance Typeable Rhs where
    typeOf rhs = rhs ^?! SL.ty

instance Typeable RuntimeType where
    typeOf = id

instance Typeable Expression where
    typeOf expression = expression ^?! SL.ty

instance Typeable Lit where
    typeOf lit = case lit of
        BoolLit{}   -> BoolRuntimeType  ; UIntLit{}  -> UIntRuntimeType 
        IntLit{}    -> IntRuntimeType   ; FloatLit{} -> FloatRuntimeType
        StringLit{} -> StringRuntimeType; CharLit{}  -> CharRuntimeType
        NullLit{}   -> REFRuntimeType

instance Typeable NonVoidType where
    typeOf ty = case ty of
        UIntType{}      -> UIntRuntimeType
        IntType{}       -> IntRuntimeType
        FloatType{}     -> FloatRuntimeType
        BoolType{}      -> BoolRuntimeType
        StringType{}    -> StringRuntimeType   
        CharType{}      -> CharRuntimeType
        ReferenceType{} -> ReferenceRuntimeType (ty ^?! SL.ty)
        ArrayType{}     -> ArrayRuntimeType (typeOf (ty ^?! SL.innerTy))

instance Typeable Type where
    typeOf ty = maybe VoidRuntimeType typeOf (ty ^?! SL.ty)
