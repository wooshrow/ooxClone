module Execution.Semantics.StackFrame where

import qualified Data.Stack as T
import qualified Data.Map as M
import           Text.Pretty
import           Control.Lens hiding (assign)
import           Execution.State
import           Execution.State.Thread
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
                stop state "writeDeclaration: no stack frame"
    | otherwise = 
        stop state "writeDeclaration: cannot get current thread"

writeDeclarationOnFrame :: StackFrame -> Identifier -> Expression -> StackFrame
writeDeclarationOnFrame frame var value = frame & (declarations %~ M.insert var value)

readDeclaration :: ExecutionState -> Identifier -> Engine r Expression
readDeclaration state var
    | Just thread <- getCurrentThread state =
        case getLastStackFrame thread of
            Just frame -> do
                let value = (frame ^. declarations) M.!? var
                maybe (stop state (readDeclarationErrorMessage var)) return value
            Nothing    ->
                stop state "readDeclaration: no stack frame"
    | otherwise = 
        stop state "readDeclaration: cannot get current thread"

getLastStackFrame :: Thread -> Maybe StackFrame
getLastStackFrame thread = T.peek (thread ^. callStack)

readDeclarationErrorMessage :: Identifier -> String
readDeclarationErrorMessage (Identifier var pos) =
    "readDeclaration: failed to read variable '" ++ var ++ "' declared at " ++
    "'" ++ toString pos