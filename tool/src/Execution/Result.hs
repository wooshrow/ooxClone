module Execution.Result where

import Prelude hiding ((<>))
import Text.Pretty
import Data.Error
import Data.Positioned
import Analysis.CFA.CFG

data VerificationResult
    = Valid
    | Invalid Position [CFGContext]
    | Unknown Position [CFGContext]
    | Deadlock [CFGContext]
    | InternalError ErrorMessage

isValid, isInvalid, isUnknown, isDeadlock, isInternalError :: VerificationResult -> Bool
isValid         result = case result of Valid{}         -> True; _ -> False  
isInvalid       result = case result of Invalid{}       -> True; _ -> False  
isUnknown       result = case result of Unknown{}       -> True; _ -> False  
isDeadlock      result = case result of Deadlock{}      -> True; _ -> False  
isInternalError result = case result of InternalError{} -> True; _ -> False  

instance Pretty [VerificationResult] where
    pretty []       = empty
    pretty (vc:vcs) = pretty vc $+$ pretty vcs

instance Pretty VerificationResult where
    pretty Valid = 
        text "VALID"
    pretty (Invalid pos _) = 
        text "INVALID assertion" <+> pretty pos
    pretty (Unknown pos _) = 
        text "UNKNOWN assertion" <+> pretty pos
    pretty (Deadlock _) = 
        text "DEADLOCK"
    pretty (InternalError message) = 
        text "INTERNAL ERROR '" <> pretty message <> text "'"

    prettyDebug (InternalError message) = 
        text "INTERNAL ERROR '" <> prettyDebug message <> text "'"
    prettyDebug result =
        pretty result