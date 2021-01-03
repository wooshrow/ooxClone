module Execution.Errors where

import qualified GHC.Stack as GHC
import Data.Error
import Text.Pretty (toString)
import Language.Syntax
import Execution.State.Thread (ThreadId)

cannotGetCurrentThreadErrorMessage :: GHC.HasCallStack => ErrorMessage
cannotGetCurrentThreadErrorMessage = otherError
    "cannot get the current thread"
    
cannotGetThreadErrorMessage :: GHC.HasCallStack => ThreadId -> ErrorMessage
cannotGetThreadErrorMessage tid = otherError $
    "cannot get the thread with thread id '" ++ toString tid ++ "'"

cannotGetCurrentStackFrameErrorMessage :: GHC.HasCallStack =>  ErrorMessage
cannotGetCurrentStackFrameErrorMessage = otherError
    "no stack frame"

readOfUndeclaredVariableErrorMessage :: GHC.HasCallStack => Identifier -> ErrorMessage
readOfUndeclaredVariableErrorMessage var = otherError $
    "failed to read variable '" ++ toString var ++ "'"

readOfUndeclaredFieldErrorMessge :: GHC.HasCallStack => Identifier -> ErrorMessage
readOfUndeclaredFieldErrorMessge field = otherError $
    "failed to read field '" ++ toString field ++ "'"

expectedReferenceErrorMessage :: GHC.HasCallStack => Expression -> ErrorMessage
expectedReferenceErrorMessage value = otherError $
    "expected a reference got '" ++ toString value ++ "'"

expectedObjectErrorMessage :: GHC.HasCallStack => ErrorMessage
expectedObjectErrorMessage = otherError 
    "expected an object structure got an array structure"
    
expectedArrayErrorMessage :: GHC.HasCallStack => ErrorMessage
expectedArrayErrorMessage = otherError 
    "expected an array structure got an object structure"
    
expectedSymbolicReferenceErrorMessage :: GHC.HasCallStack => Expression -> ErrorMessage
expectedSymbolicReferenceErrorMessage value = otherError $
    "expected a symbolic reference got '" ++ toString value ++ "'"

expectedConcreteReferenceErrorMessage :: GHC.HasCallStack => Expression -> ErrorMessage
expectedConcreteReferenceErrorMessage value = otherError $
    "expected a concrete reference got '" ++ toString value ++ "'"

expectedConcreteValueErrorMessge :: GHC.HasCallStack => Expression -> ErrorMessage
expectedConcreteValueErrorMessge value = otherError $
    "expected a concrete value got '" ++ toString value ++ "'"

expectedMethodMemberErrorMessage :: GHC.HasCallStack => Identifier -> ErrorMessage
expectedMethodMemberErrorMessage field = otherError $
    "expected a method value got field '" ++ toString field ++ "'"

uninitializedReferenceErrorMessage :: GHC.HasCallStack => Reference -> ErrorMessage
uninitializedReferenceErrorMessage value = otherError $
    "dereference of unitialized reference '" ++ toString value ++ "'"

noAliasesErrorMessage :: GHC.HasCallStack => ErrorMessage
noAliasesErrorMessage = otherError
    "no aliases in the alias map"

exactlyOneAliasErrorMessage :: GHC.HasCallStack => Int -> ErrorMessage
exactlyOneAliasErrorMessage n = otherError $
    "expected exactly 1 alias in the alias map got '" ++ toString n ++ "'"

unsupportedOperatorErrorMessage :: GHC.HasCallStack => ErrorMessage
unsupportedOperatorErrorMessage = otherError
    "unsupported operator"

expectedNoMethodCallErrorMessage :: GHC.HasCallStack => ErrorMessage
expectedNoMethodCallErrorMessage = otherError
    "expected a non method call rhs"

unresolvedErrorMessage :: GHC.HasCallStack => ErrorMessage
unresolvedErrorMessage = otherError
    "unresolved method call"

expectedNumberOfNeighboursErrorMessage :: GHC.HasCallStack => Int -> Int -> ErrorMessage
expectedNumberOfNeighboursErrorMessage expected actual =  otherError $
    "expected '" ++ toString expected ++ "' neighbours got '" ++ toString actual ++ "'" 
