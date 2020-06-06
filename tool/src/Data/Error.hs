module Data.Error where

import           Prelude                   hiding ((<>))
import           Data.List                        (intercalate)
import           Control.Lens.Extras
import           Data.Positioned
import           Text.Pretty
import           Language.Syntax
import           Language.Syntax.Pretty()
import qualified Language.Syntax.Lenses   as SL

type Erroneous a = Either ErrorMessage a

data ErrorMessage
    = ErrorMessage { position :: Maybe Position, err :: ErrorType }
    deriving (Show)

instance Pretty ErrorMessage where
    pretty ErrorMessage{..}
        = text "Error" <+> maybe empty pretty position <> colon 
        $+$ tab (pretty err)

data ErrorType
    = LexicalError     { message :: String }
    | SyntacticalError { message :: String }
    | SemanticalError  { message :: String }
    deriving (Show)

instance Pretty ErrorType where
    pretty LexicalError{..}     = pretty message
    pretty SyntacticalError{..} = pretty message
    pretty SemanticalError{..}  = pretty message

lexicalError :: Position -> String -> ErrorMessage
lexicalError pos = ErrorMessage (Just pos) . LexicalError 

syntacticalError :: Position -> String -> ErrorMessage
syntacticalError pos = ErrorMessage (Just pos) . SyntacticalError

readOfUndeclaredVarError :: Identifier -> ErrorMessage
readOfUndeclaredVarError (Identifier var pos) 
    = ErrorMessage (Just pos) $ SemanticalError
        ("Read of undeclared variable '" ++ var ++ "'")

writeToUndeclaredVarError :: Identifier -> ErrorMessage
writeToUndeclaredVarError (Identifier var pos) 
    = ErrorMessage (Just pos) $ SemanticalError
        ("Write to undeclared variable '" ++ var ++ "'")

unificationError :: Position -> RuntimeType -> RuntimeType -> ErrorMessage
unificationError pos expected actual
    = ErrorMessage (Just pos) $ SemanticalError
        ("Expected type '" ++ toString expected ++ "' but is of type '" ++ toString actual  ++ "'")

shadowError :: Identifier -> Identifier -> ErrorMessage
shadowError var (Identifier _ oldPos)
    = ErrorMessage (Just $ getPos var) $ SemanticalError
        ("Variable '" ++ toString var ++ "' shadows variable declared at line '"
        ++ show (sourceLine oldPos) ++ "' and column '" ++ show (sourceColumn oldPos) ++ "'")

unresolvedMethodError :: Identifier -> Identifier -> RuntimeType -> [RuntimeType] -> ErrorMessage
unresolvedMethodError className methodName retType argTypes
    = ErrorMessage (Just $ getPos className) $ SemanticalError 
        ("Unable to resolve method '" ++ retTypeString ++ toString className ++ "."
        ++ toString methodName ++ "(" ++ argTypeString ++ ")'")
    where
        argTypeString = (intercalate "," . map toString) argTypes
        retTypeString = if retType == ANYRuntimeType 
                            then "" 
                            else toString retType ++ " "

duplicateResolvedMethodError :: Identifier -> Identifier -> RuntimeType -> [RuntimeType] -> ErrorMessage
duplicateResolvedMethodError className methodName retType argTypes
    = ErrorMessage (Just $ getPos className) $ SemanticalError 
        ("Multiple methods match '"++ retTypeString ++ toString className ++ "."
        ++ toString methodName ++ "(" ++ argTypeString ++ ")'")
    where
        argTypeString = (intercalate "," . map toString) argTypes
        retTypeString = if retType == ANYRuntimeType 
                            then "" 
                            else toString retType ++ " "

unresolvedFieldError :: Identifier -> Identifier -> ErrorMessage
unresolvedFieldError (Identifier className _) (Identifier fieldName pos)
    = ErrorMessage (Just pos) $ SemanticalError
        ("Unable te resolve field '" ++ className ++ "." ++ fieldName ++ "'")

continueOrBreakNotInWhileError :: Statement -> ErrorMessage
continueOrBreakNotInWhileError stat
    = ErrorMessage (Just $ getPos stat) $ SemanticalError
        ("Statement '" ++ statementString ++ "' must be contained in the body"
        ++ " of a while statement")
    where
        statementString
            | SL._Break    `is` stat = "break;"
            | SL._Continue `is` stat = "continue;"
            | otherwise              = error "'continueOrBreakNotInWhile' called with a statement that is not a 'continue' or 'break'"

expectedReturnValueError :: RuntimeType -> Position -> ErrorMessage
expectedReturnValueError expectedType pos 
    = ErrorMessage (Just pos) $ SemanticalError
        ("Expected a return statement with an expression of type '" 
        ++ toString expectedType ++ "', instead no expression was given")

unexpectedReturnValueError :: Expression -> ErrorMessage
unexpectedReturnValueError expression
    = ErrorMessage (Just (getPos expression)) $ SemanticalError
        ("Expected a return statement without an expression, instead 'return " 
        ++ toString expression ++ "' was given")

unknownEntryPointError :: String -> ErrorMessage
unknownEntryPointError entryPoint
    = ErrorMessage Nothing $ SemanticalError
        ("Unknown entry point '" ++ entryPoint ++ "', the entry point must "
        ++ "be of the form 'class.method' and be unambiguous")