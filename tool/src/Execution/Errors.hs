module Execution.Errors where

import Data.Error
import Text.Pretty (toString)
import Language.Syntax
import Execution.State.Thread

cannotGetCurrentThreadErrorMessage :: String -> ErrorMessage
cannotGetCurrentThreadErrorMessage caller = otherError $ withCallerName caller
    "cannot get the current thread"
    
cannotGetThreadErrorMessage :: String -> ThreadId -> ErrorMessage
cannotGetThreadErrorMessage caller tid = otherError $ withCallerName caller
    "cannot get the thread with thread id '" ++ toString tid ++ "'"

cannotGetCurrentStackFrameErrorMessage :: String -> ErrorMessage
cannotGetCurrentStackFrameErrorMessage caller = otherError $ withCallerName caller
    "no stack frame"

readOfUndeclaredVariableErrorMessage :: String -> Identifier -> ErrorMessage
readOfUndeclaredVariableErrorMessage caller var = otherError $ withCallerName caller
    "failed to read variable '" ++ toString var ++ "'"

readOfUndeclaredFieldErrorMessge :: String -> Identifier -> ErrorMessage
readOfUndeclaredFieldErrorMessge caller field = otherError $ withCallerName caller
    "failed to read field '" ++ toString field ++ "'"

expectedReferenceErrorMessage :: String -> Expression -> ErrorMessage
expectedReferenceErrorMessage caller value = otherError $ withCallerName caller
    "expected a reference got '" ++ toString value ++ "'"

expectedObjectErrorMessage :: String -> ErrorMessage
expectedObjectErrorMessage caller = otherError $ withCallerName caller
    "expected an object structure got an array structure"
    
expectedArrayErrorMessage :: String -> ErrorMessage
expectedArrayErrorMessage caller = otherError $ withCallerName caller
    "expected an array structure got an object structure"
    
expectedSymbolicReferenceErrorMessage :: String -> Expression -> ErrorMessage
expectedSymbolicReferenceErrorMessage caller value = otherError $ withCallerName caller
    "expected a symbolic reference got '" ++ toString value ++ "'"

expectedConcreteReferenceErrorMessage :: String -> Expression -> ErrorMessage
expectedConcreteReferenceErrorMessage caller value = otherError $ withCallerName caller
    "expected a concrete reference got '" ++ toString value ++ "'"

expectedConcreteValueErrorMessge :: String -> Expression -> ErrorMessage
expectedConcreteValueErrorMessge caller value = otherError $ withCallerName caller
    "expected a concrete value got '" ++ toString value ++ "'"

expectedMethodMemberErrorMessage :: String -> Identifier -> ErrorMessage
expectedMethodMemberErrorMessage caller field = otherError $ withCallerName caller
    "expected a method value got field '" ++ toString field ++ "'"

uninitializedReferenceErrorMessage :: String -> Reference -> ErrorMessage
uninitializedReferenceErrorMessage caller value = otherError $ withCallerName caller
    "dereference of unitialized reference '" ++ toString value ++ "'"

noAliasesErrorMessage :: String -> ErrorMessage
noAliasesErrorMessage caller = otherError $ withCallerName caller
    "no aliases in the alias map"

exactlyOneAliasErrorMessage :: String -> Int -> ErrorMessage
exactlyOneAliasErrorMessage caller n = otherError $ withCallerName caller
    "expected exactly 1 alias in the alias map got '" ++ toString n ++ "'"

unsupportedOperatorErrorMessage :: String -> ErrorMessage
unsupportedOperatorErrorMessage caller = otherError $ withCallerName caller
    "unsupported operator"

expectedNoMethodCallErrorMessage :: String -> ErrorMessage
expectedNoMethodCallErrorMessage caller = otherError $ withCallerName caller
    "expected a non method call rhs"

unresolvedErrorMessage :: String -> ErrorMessage
unresolvedErrorMessage caller = otherError $ withCallerName caller
    "unresolved method call"

expectedNumberOfNeighboursErrorMessage :: String -> Int -> Int -> ErrorMessage
expectedNumberOfNeighboursErrorMessage caller expected actual =  otherError $ withCallerName caller
    "expected '" ++ toString expected ++ "' neighbours got '" ++ toString actual ++ "'" 

--------------------------------------------------------------------------------
-- Utility functions
    
withCallerName :: String -> String -> String
withCallerName caller message = caller ++ ": " ++ message