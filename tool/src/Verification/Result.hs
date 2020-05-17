module Verification.Result where

import Prelude hiding ((<>))
import Text.Pretty
import Data.Positioned
import Syntax.Syntax
import Syntax.Pretty()
import Analysis.CFA.CFG

data VerificationResult
    = Valid
    | Invalid SourcePos [CFGContext]
    | Unknown SourcePos [CFGContext]
    | Deadlock [CFGContext]
    | Infeasible

isValid, isInvalid, isUnknown, isDeadlock :: VerificationResult -> Bool
isValid    result = case result of Valid{}    -> True; _ -> False  
isInvalid  result = case result of Invalid{}  -> True; _ -> False  
isUnknown  result = case result of Unknown{}  -> True; _ -> False  
isDeadlock result = case result of Deadlock{} -> True; _ -> False  

instance Pretty [VerificationResult] where
    pretty [] = empty
    pretty (vc:vcs) = pretty vc $+$ pretty vcs

instance Pretty VerificationResult where
    pretty Valid                      = text "VALID"
    pretty (Invalid pos programTrace) = text "INVALID assertion" <+> pretty pos -- $+$ (tab (pretty programTrace)
    pretty (Unknown pos programTrace) = text "UNKNOWN assertion" <+> pretty pos -- $+$ tab (pretty programTrace)
    pretty (Deadlock programTrace)    = text "DEADLOCK" -- $+$ tab (pretty programTrace)
    pretty Infeasible                 = text "INFEASIBLE"

instance Pretty [CFGContext] where
    pretty = mconcat . map prettyTraceValue

prettyTraceValue :: CFGContext -> Doc
prettyTraceValue (_, _, (StatNode Ite{})  , _)     = empty
prettyTraceValue (_, _, (StatNode While{}), _)     = empty
prettyTraceValue (_, _, (StatNode stat), _)        = pretty stat
prettyTraceValue (_, _, node@CallNode{}, _)        = pretty node 
prettyTraceValue (_, _, node@ForkNode{}, _)        = pretty node 
prettyTraceValue (_, _, node@MemberEntry{}, _)     = pretty node 
prettyTraceValue (_, _, node@MemberExit{}, _)      = pretty node 
prettyTraceValue (_, _, node@ExceptionalNode{}, _) = pretty node 
prettyTraceValue (_, _, node@TryEntry{}, _)        = pretty node 
prettyTraceValue (_, _, node@TryExit{}, _)         = pretty node 
prettyTraceValue (_, _, node@CatchEntry{}, _)      = pretty node 
prettyTraceValue (_, _, node@CatchExit{}, _)       = pretty node 