module Execution.Semantics.StackFrame where

import qualified Data.Stack as T
import qualified Data.Map as M
import qualified Data.Set as S
import           Text.Pretty
import           Control.Lens ((&), (^.), (%~))
import           Execution.Effects
import           Execution.State
import           Execution.State.Thread
import           Execution.State.AliasMap as AliasMap
import           Language.Syntax

writeDeclaration :: ExecutionState -> Identifier -> Expression -> Engine r ExecutionState
writeDeclaration state var value 
    | Just thread0 <- getCurrentThread state =
        case getLastStackFrame thread0 of
            Just oldFrame -> do
                debug ("Assigning '" ++ toString value ++ "' to '" ++ toString var ++ "'")
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
            Just frame ->
                case (frame ^. declarations) M.!? var of
                    Nothing ->
                        stop state ("readDeclaration: failed to read variable '" ++ toString var ++ "')")
                    Just value@(SymbolicRef ref _ _) ->
                        case AliasMap.lookup ref (state ^. aliasMap) of
                            Nothing -> 
                                return value
                            Just aliases ->
                                if S.size aliases == 1
                                    then return $ S.elemAt 0 aliases
                                    else return value
                    Just value ->
                        return value
            Nothing    ->
                stop state "readDeclaration: no stack frame"
    | otherwise = 
        stop state "readDeclaration: cannot get current thread"

getLastStackFrame :: Thread -> Maybe StackFrame
getLastStackFrame thread = T.peek (thread ^. callStack)
