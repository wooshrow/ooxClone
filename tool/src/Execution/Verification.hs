module Execution.Verification(
      verifyM
    , verify
) where

import qualified Data.Set as S
import           Polysemy
import           Z3.Monad hiding (substitute)
import           Control.Monad (void)
import           Control.Lens
import           Data.Positioned
import           Data.Configuration
import           Data.Statistics
import           Execution.Effects
import           Execution.Errors
import           Execution.Semantics.Evaluation
import           Execution.State
import           Execution.State.AliasMap as AliasMap
import           Language.Syntax
import           Language.Syntax.Fold
import           Execution.Semantics.Concretization
import qualified Language.Syntax.Lenses as SL
import           Language.Syntax.Pretty()

--------------------------------------------------------------------------------
-- Verification Interface
--------------------------------------------------------------------------------

verifyM :: ExecutionState -> Maybe Expression -> Engine r ExecutionState
verifyM state = maybe (return state) (verify state)

verify :: ExecutionState -> Expression -> Engine r ExecutionState
verify state expression = do
    config <- askConfig
    if cacheFormulas config
        then do 
            isCached <- cached expression
            if isCached 
                then measureCacheHit >> return state
                else store expression >> verify' state expression
        else verify' state expression

verify' :: ExecutionState -> Expression -> Engine r ExecutionState
verify' state0 expression0 = do
    (state1, concretizations) <- concretesOfType state0 REFRuntimeType expression0
    void $ concretizeMap concretizations state1 $ \ state2 -> do
        expression1 <- substitute state2 expression0
        (state3, expression2) <- evaluateAsBool state2 expression1
        case expression2 of
            Right True ->
                invalid state3 expression1
            Right False ->
                return state3
            Left expression3 -> do
                measureInvokeZ3
                (result, _) <- (embed . evalZ3 . verifyZ3) expression3
                case result of
                    Unsat -> return state3
                    Sat   -> invalid state3 expression3
                    Undef -> unknown state3 expression3
    return state0

substitute :: ExecutionState -> Expression -> Engine r Expression
substitute state = foldExpression algebra
    where
        algebra = identityMExpressionAlgebra
            { fSymRef = substituteSymbolicRef state }

substituteSymbolicRef :: ExecutionState -> Identifier -> RuntimeType -> Position -> Engine r Expression
substituteSymbolicRef state ref _ _
    | Just aliases <- AliasMap.lookup ref (state ^. aliasMap) =
        case S.size aliases of
            1 -> return $ S.elemAt 0 aliases
            n -> stop state (exactlyOneAliasErrorMessage "substituteSymbolicRef" n)
    | otherwise =
        stop state (noAliasesErrorMessage "substituteSymbolicRef")

--------------------------------------------------------------------------------
-- Verification Engine
--------------------------------------------------------------------------------

verifyZ3 :: Expression -> Z3 (Result, Maybe Model)
verifyZ3 formula = (assert =<< construct formula) >> solverCheckAndGetModel

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