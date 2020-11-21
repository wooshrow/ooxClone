module Execution.Semantics.StackFrame where

import qualified Data.Stack as T
import qualified Data.Map as M
import           Text.Pretty
import           Polysemy.Error
import           Control.Lens hiding (assign)
import           Execution.State
import           Execution.State.Thread
import           Verification.Result
import           Language.Syntax

writeDeclaration :: ExecutionState -> Identifier -> Expression -> Engine r ExecutionState
writeDeclaration state var value 
    | Just thread0 <- getCurrentThread state =
        case getLastStackFrame thread0 of
            Just oldFrame -> do
                let newFrame = writeDeclarationOnFrame oldFrame var value
                let thread1  = thread0 & (callStack %~ T.updateTop newFrame)
                return $ updateThreadInState state thread1
            Nothing       -> 
                throw (InternalError "writeDeclaration: no stack frame")
    | otherwise = 
        throw (InternalError "writeDeclaration: cannot get current thread")

writeDeclarationOnFrame :: StackFrame -> Identifier -> Expression -> StackFrame
writeDeclarationOnFrame frame var value = frame & (declarations %~ M.insert var value)

readDeclaration :: ExecutionState -> Identifier -> Engine r Expression
readDeclaration state var
    | Just thread <- getCurrentThread state =
        case getLastStackFrame thread of
            Just frame -> do
                let value = (frame ^. declarations) M.!? var
                maybe (throw (InternalError (readDeclarationErrorMessage var frame))) return value
            Nothing    ->
                throw (InternalError "readDeclaration: no stack frame")
    | otherwise = 
        throw (InternalError "readDeclaration: cannot get current thread")

getLastStackFrame :: Thread -> Maybe StackFrame
getLastStackFrame thread = T.peek (thread ^. callStack)

readDeclarationErrorMessage :: Identifier -> StackFrame -> String
readDeclarationErrorMessage (Identifier var pos) frame =
    "readDeclaration: failed to read variable '" ++ var ++ "' declared at " ++
    "'" ++ toString pos ++ "' with stack frame " ++ toString frame