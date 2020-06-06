module Verification.Verifier(
      verifyM
    , verify
    , cartesianProduct
) where

import qualified Data.Map                   as M
import qualified Data.Set                   as S
import           Data.Maybe
import           Control.Monad
import           Polysemy
import           Polysemy.Error
import           Polysemy.State
import           Polysemy.Reader
import           Z3.Monad
import           Control.Lens
import           Text.Pretty
import           Data.Positioned
import           Data.Statistics
import           Data.Configuration
import           Verification.Result
import           Analysis.CFA.CFG
import           Analysis.Type.Typeable
import           Execution.Memory.AliasMap
import           Language.Syntax
import           Language.Syntax.Fold
import qualified Language.Syntax.Lenses    as SL
import           Language.Syntax.Pretty()

--------------------------------------------------------------------------------
-- Verification Interface
--------------------------------------------------------------------------------

verifyM  :: (HasConfiguration a, Members [State Statistics, Reader a, Embed IO, Error VerificationResult] r)
    => [CFGContext] -> M.Map Identifier (S.Set Expression) -> Position -> Maybe Expression -> Sem r ()
verifyM programTrace aliases pos = maybe (return ()) (verify programTrace aliases pos)

verify :: (HasConfiguration a, Members [State Statistics, Reader a, Embed IO, Error VerificationResult] r)
    => [CFGContext] -> M.Map Identifier (S.Set Expression) -> Position -> Expression -> Sem r ()
verify programTrace aliases pos = void . verifyEach programTrace pos . concretize aliases

verifyEach :: (HasConfiguration a, Members [State Statistics, Reader a, Embed IO, Error VerificationResult] r)
    => [CFGContext] -> Position -> [Expression] -> Sem r Result
verifyEach _            _   []     = return Unsat
verifyEach programTrace pos (n:ns) = do
    debug ("Verifying: " ++ toString n)
    measureInvokeZ3
    result <- (embed . evalZ3 . verifyZ3) n
    case result of
        Unsat -> verifyEach programTrace pos ns
        Sat   -> throw $ Invalid pos programTrace
        Undef -> throw $ Unknown pos programTrace

--------------------------------------------------------------------------------
-- Verification Engine
--------------------------------------------------------------------------------

concretize :: AliasMap -> Expression -> [Expression]
concretize aliases formula
    | refs <- findSymbolicRefs formula
    , not (null refs) 
        = let mappings = map (\ ref -> map (ref ^?! SL.var, ) (maybe (error "concretize") S.toList (aliases M.!? (ref ^?! SL.var)))) (S.toList refs)
           in map (\ p -> makeConcrete (M.fromList p) formula) (cartesianProduct mappings)
    | otherwise
        = [formula]

makeConcrete :: Concretization -> Expression -> Expression
makeConcrete substitutions = foldExpression algebra
    where
        algebra = identityExpressionAlgebra
            { fSymRef = \ ref _ _ -> fromMaybe (error ("makeConcrete: " ++ toString ref)) (substitutions M.!? ref) }
        
findSymbolicRefs :: Expression -> S.Set Expression
findSymbolicRefs = foldExpression algebra
    where
        algebra = monoidExpressionAlgebra
            { fSymRef = \ symVar varTy varPos -> S.singleton (SymbolicRef symVar varTy varPos) }

verifyZ3 :: Expression -> Z3 Result
verifyZ3 formula= (assert =<< construct formula) >> check

construct :: Expression -> Z3 AST
construct e@BinOp{} = do
    lhs <- construct (e ^?! SL.lhs)
    rhs <- construct (e ^?! SL.rhs)
    case e ^?! SL.binOp of
        Implies          -> mkImplies lhs rhs     ; And         -> mkAnd [lhs, rhs]
        Or               -> mkOr [lhs, rhs]       ; Equal       -> mkEq lhs rhs
        NotEqual         -> mkEq lhs rhs >>= mkNot; LessThan    -> mkLt lhs rhs
        LessThanEqual    -> mkLe lhs rhs          ; GreaterThan -> mkGt lhs rhs
        GreaterThanEqual -> mkGe lhs rhs          ; Plus        -> mkAdd [lhs, rhs]
        Minus            -> mkSub [lhs, rhs]      ; Multiply    -> mkMul [lhs, rhs]
        Divide           -> mkDiv lhs rhs         ; Modulo      -> mkMod lhs rhs

construct e@UnOp{} = do
    value <- construct (e ^?! SL.value)
    case e ^?! SL.unOp of
        Negative -> mkUnaryMinus value
        Negate   -> mkNot value
        
construct e@SymbolicVar{} = do
    symbol <- mkStringSymbol (e ^?! SL.var ^. SL.name)
    sort   <- runtimeTypeToSort (e ^?! SL.ty)
    mkVar symbol sort

construct e@Lit{} = 
    case e ^?! SL.lit of
        lit@BoolLit{}  -> mkBool (lit ^?! SL.boolValue)
        lit@IntLit{}   -> mkIntNum (lit ^?! SL.intValue)
        lit@FloatLit{} -> mkRealNum (lit ^?! SL.floatValue)
        lit@NullLit{}  -> construct (Ref 0 REFRuntimeType (lit ^. SL.info))
        _              -> error "construct: unsupported literal"

construct e@Ref{} = mkIntNum (e ^?! SL.ref)

construct e@Conditional{} = do
    guard <- construct (e ^?! SL.guard)
    true  <- construct (e ^?! SL.true)
    false <- construct (e ^?! SL.false)
    mkIte guard true false

construct e = error $ "construct missing: " ++ show e

runtimeTypeToSort :: RuntimeType -> Z3 Sort
runtimeTypeToSort ty = case ty of
    IntRuntimeType           -> mkIntSort
    BoolRuntimeType          -> mkBoolSort
    ArrayRuntimeType innerTy -> do
        elemSort <- runtimeTypeToSort innerTy
        mkArraySort elemSort =<< mkIntSort
    _                  -> error "non-concrete or non-implemented type."

--------------------------------------------------------------------------------
-- Auxiliary functions
--------------------------------------------------------------------------------

cartesianProduct :: [[a]] -> [[a]]
cartesianProduct = foldr f [[]]
    where f l a = [ x:xs | x <- l, xs <- a ]