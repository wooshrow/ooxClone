module Execution.Semantics.AssertAssume(
      execAssert
    , execAssume
) where

import Debug.Trace
import qualified GHC.Stack as GHC
import qualified Data.Set as S
import qualified Data.Map as M
import           Data.Maybe
import           Control.Lens ((&), (^?!), (^.), (%~), (.~), (<>~))
import           Text.Pretty
--import           Data.Configuration
import           Data.Statistics
import           Data.Positioned
import           Analysis.Type.Typeable
--import           Analysis.CFA.CFG
--import           Analysis.SymbolTable
import           Language.Syntax
import           Language.Syntax.DSL
import qualified Language.Syntax.Lenses as SL
import           Execution.Semantics.Evaluation
--import           Execution.Semantics.Thread
--import           Execution.Semantics.Exception
import           Execution.Semantics.Concretization
--import           Execution.Semantics.StackFrame
--import           Execution.Semantics.Heap
--import           Execution.Semantics.Process
--import           Execution.Semantics.Assignment
import           Execution.Effects
--import           Execution.Errors
import           Execution.State
--import           Execution.State.Thread
--import           Execution.State.Heap
import           Execution.State.PathConstraints as PathConstraints
--import           Execution.State.LockSet as LockSet
import           Execution.Verification

execAssert :: GHC.HasCallStack => ExecutionState -> Expression -> Engine r ExecutionState
execAssert state0 assertion = do
    measureVerification
    let assumptions = state0 ^. constraints
    (state1, concretizations) <- concretesOfType state0 ARRAYRuntimeType assertion
    concretize concretizations state1 $ \ state2 -> do
        let formula0 = neg' (implies' (asExpression assumptions) assertion)
        debug ("Verifying: '" ++ toString formula0 ++ "'")
        (state3, formula1) <- evaluateAsBool state2 formula0
        case formula1 of
            Right True ->
                invalid state3 assertion
            Right False ->
                return state3
            Left formula2 -> do
                _ <- verify state3 (formula2 & SL.info .~ getPos assertion)
                return state3

execAssume :: GHC.HasCallStack => ExecutionState -> Expression -> Engine r ExecutionState
execAssume state0 assumption0 = do
    (state1, concretizations) <- concretesOfType state0 ARRAYRuntimeType assumption0
    concretize concretizations state1 $ \ state2 -> do
        (state3, assumption1) <- evaluateAsBool state2 assumption0
        case assumption1 of
            Right True  ->
                return state3
            Right False -> do
                debug "Constraint is infeasible"
                infeasible
            Left assumption2 -> do
                debug ("Adding constraint: '" ++ toString assumption2 ++ "'")
                return $ state3 & (constraints <>~ PathConstraints.singleton assumption2)
