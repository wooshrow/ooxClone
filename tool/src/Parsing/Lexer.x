{
module Parsing.Lexer (
      lexer
    , Token(..)
) where

import Text.Pretty
import Data.Positioned
import Data.Error
}

%wrapper "posn"

$digit     = [0-9]
$alpha     = [a-zA-Z]
@character = $digit | $alpha | "\\" | "\’" | "\0" | "\a" | \b 
           | "\f"   | "\n"   | "\r" | "\t" | "\v"
@comment   = "//".*

tokens :-
    $white+     ;
    @comment    ;

    "assert"        { \ (AlexPn _ l c) _ -> Positioned (newPos "" l c) TAssert      }
    "assume"        { \ (AlexPn _ l c) _ -> Positioned (newPos "" l c) TAssume      }
    "bool"          { \ (AlexPn _ l c) _ -> Positioned (newPos "" l c) TBool        }
    "break"         { \ (AlexPn _ l c) _ -> Positioned (newPos "" l c) TBreak       }
    "catch"         { \ (AlexPn _ l c) _ -> Positioned (newPos "" l c) TCatch       }
    "char"          { \ (AlexPn _ l c) _ -> Positioned (newPos "" l c) TChar        }
    "class"         { \ (AlexPn _ l c) _ -> Positioned (newPos "" l c) TClass       }
    "const"         { \ (AlexPn _ l c) _ -> Positioned (newPos "" l c) TConst       }
    "continue"      { \ (AlexPn _ l c) _ -> Positioned (newPos "" l c) TContinue    }
    "else"          { \ (AlexPn _ l c) _ -> Positioned (newPos "" l c) TElse        }
    "ensures"       { \ (AlexPn _ l c) _ -> Positioned (newPos "" l c) TEnsures     }
    "exceptional"   { \ (AlexPn _ l c) _ -> Positioned (newPos "" l c) TExceptional }
    "exists"        { \ (AlexPn _ l c) _ -> Positioned (newPos "" l c) TExists      }
    "false"         { \ (AlexPn _ l c) _ -> Positioned (newPos "" l c) TFalse       }
    "finally"       { \ (AlexPn _ l c) _ -> Positioned (newPos "" l c) TFinally     }
    "float"         { \ (AlexPn _ l c) _ -> Positioned (newPos "" l c) TFloat       }
    "forall"        { \ (AlexPn _ l c) _ -> Positioned (newPos "" l c) TForall      }
    "fork"          { \ (AlexPn _ l c) _ -> Positioned (newPos "" l c) TFork        }
    "if"            { \ (AlexPn _ l c) _ -> Positioned (newPos "" l c) TIf          }
    "int"           { \ (AlexPn _ l c) _ -> Positioned (newPos "" l c) TInt         }
    "join"          { \ (AlexPn _ l c) _ -> Positioned (newPos "" l c) TJoin        }
    "lock"          { \ (AlexPn _ l c) _ -> Positioned (newPos "" l c) TLock        }
    "new"           { \ (AlexPn _ l c) _ -> Positioned (newPos "" l c) TNew         }
    "null"          { \ (AlexPn _ l c) _ -> Positioned (newPos "" l c) TNull        }
    "requires"      { \ (AlexPn _ l c) _ -> Positioned (newPos "" l c) TRequires    }
    "return"        { \ (AlexPn _ l c) _ -> Positioned (newPos "" l c) TReturn      }
    "static"        { \ (AlexPn _ l c) _ -> Positioned (newPos "" l c) TStatic      }
    "string"        { \ (AlexPn _ l c) _ -> Positioned (newPos "" l c) TString      }
    "throw"         { \ (AlexPn _ l c) _ -> Positioned (newPos "" l c) TThrow       }
    "true"          { \ (AlexPn _ l c) _ -> Positioned (newPos "" l c) TTrue        }
    "try"           { \ (AlexPn _ l c) _ -> Positioned (newPos "" l c) TTry         }
    "uint"          { \ (AlexPn _ l c) _ -> Positioned (newPos "" l c) TUint        }
    "void"          { \ (AlexPn _ l c) _ -> Positioned (newPos "" l c) TVoid        }
    "while"         { \ (AlexPn _ l c) _ -> Positioned (newPos "" l c) TWhile       }

    [$digit]+ "." [$digit]+
        { \ (AlexPn _ l c) v -> Positioned (newPos "" l c) (TRealLiteral (read v)) }

    [$digit]+ { \ (AlexPn _ l c) v -> Positioned (newPos "" l c) (TIntLiteral (read v)) }

    "'" @character "'" { \ (AlexPn _ l c) v -> Positioned (newPos "" l c) (TCharLiteral (init (tail v))) }

    \" .+ \" { \ (AlexPn _ l c) v -> Positioned (newPos "" l c) (TStringLiteral (init (tail v))) }

    "==>"   { \ (AlexPn _ l c) _ -> Positioned (newPos "" l c) TImplies          }
    "+"     { \ (AlexPn _ l c) _ -> Positioned (newPos "" l c) TPlus             }
    "-"     { \ (AlexPn _ l c) _ -> Positioned (newPos "" l c) TMinus            }
    "*"     { \ (AlexPn _ l c) _ -> Positioned (newPos "" l c) TMultiply         }
    "/"     { \ (AlexPn _ l c) _ -> Positioned (newPos "" l c) TDivide           }
    "%"     { \ (AlexPn _ l c) _ -> Positioned (newPos "" l c) TModulo           }
    "!"     { \ (AlexPn _ l c) _ -> Positioned (newPos "" l c) TNot              }
    "#"     { \ (AlexPn _ l c) _ -> Positioned (newPos "" l c) TSizeOf           }
    ":="    { \ (AlexPn _ l c) _ -> Positioned (newPos "" l c) TAssign           }
    "<="    { \ (AlexPn _ l c) _ -> Positioned (newPos "" l c) TLessThanEqual    }
    ">="    { \ (AlexPn _ l c) _ -> Positioned (newPos "" l c) TGreaterThanEqual }
    "<"     { \ (AlexPn _ l c) _ -> Positioned (newPos "" l c) TLessThan         }
    ">"     { \ (AlexPn _ l c) _ -> Positioned (newPos "" l c) TGreaterThan      }
    "=="    { \ (AlexPn _ l c) _ -> Positioned (newPos "" l c) TEqual            }
    "!="    { \ (AlexPn _ l c) _ -> Positioned (newPos "" l c) TNotEqual         }
    "&&"    { \ (AlexPn _ l c) _ -> Positioned (newPos "" l c) TAnd              }
    "||"    { \ (AlexPn _ l c) _ -> Positioned (newPos "" l c) TOr               }
    "{"     { \ (AlexPn _ l c) _ -> Positioned (newPos "" l c) TCOpen            }
    "}"     { \ (AlexPn _ l c) _ -> Positioned (newPos "" l c) TCClose           }
    "["     { \ (AlexPn _ l c) _ -> Positioned (newPos "" l c) TSOpen            }
    "]"     { \ (AlexPn _ l c) _ -> Positioned (newPos "" l c) TSClose           }
    "("     { \ (AlexPn _ l c) _ -> Positioned (newPos "" l c) TPOpen            }
    ")"     { \ (AlexPn _ l c) _ -> Positioned (newPos "" l c) TPClose           }
    "."     { \ (AlexPn _ l c) _ -> Positioned (newPos "" l c) TDot              }
    ","     { \ (AlexPn _ l c) _ -> Positioned (newPos "" l c) TComma            }
    ";"     { \ (AlexPn _ l c) _ -> Positioned (newPos "" l c) TSemicolon        }
    ":"     { \ (AlexPn _ l c) _ -> Positioned (newPos "" l c) TColon            }

    [$alpha]+ { \ (AlexPn _ l c) s -> Positioned (newPos "" l c) (TIdentifier s) }

{
data Token
    = TAssert
    | TAssume
    | TBool
    | TBreak
    | TCatch
    | TChar
    | TClass
    | TConst
    | TContinue
    | TElse
    | TEnsures
    | TExceptional
    | TExists
    | TFalse
    | TFinally
    | TFloat
    | TForall
    | TFork
    | TIf
    | TInt
    | TJoin
    | TLock
    | TNew
    | TNull
    | TRequires
    | TReturn
    | TStatic
    | TString
    | TThrow
    | TTrue
    | TTry
    | TUint
    | TVoid
    | TWhile
    | TPlus
    | TMinus
    | TMultiply
    | TDivide
    | TModulo
    | TNot
    | TSizeOf
    | TAssign
    | TLessThan
    | TGreaterThan
    | TLessThanEqual
    | TGreaterThanEqual
    | TEqual
    | TNotEqual
    | TAnd
    | TOr
    | TImplies
    | TCOpen
    | TCClose
    | TSOpen
    | TSClose
    | TPOpen
    | TPClose
    | TDot
    | TComma
    | TSemicolon
    | TColon
    | TIntLiteral Int
    | TRealLiteral Float
    | TCharLiteral String
    | TStringLiteral String
    | TIdentifier String
    deriving (Show, Eq)

lexer :: FilePath -> String -> Erroneous [Positioned Token]
lexer fileName str = go (alexStartPos, '\n', [], str)
  where
    go inp@(pos,_,_,str') =
          case alexScan inp 0 of
                AlexEOF -> return []
                AlexError ((AlexPn _ line column),_,_,_) -> Left $ lexicalError (newPos fileName line column) ""
                AlexSkip  inp' _       -> go inp'
                AlexToken inp' len act -> do
                    let tok = act pos (take len str') 
                    rest <- go inp'
                    return (tok : rest)

instance Pretty Token where
    pretty TAssert            = text "keyword assert" 
    pretty TAssume            = text "keyword assume"
    pretty TBool              = text "keyword bool"
    pretty TBreak             = text "keyword break"
    pretty TCatch             = text "keyword catch"
    pretty TChar              = text "keyword char"
    pretty TClass             = text "keyword class"
    pretty TConst             = text "keyword const"
    pretty TContinue          = text "keyword continue"
    pretty TElse              = text "keyword else"
    pretty TEnsures           = text "keyword ensures"
    pretty TExceptional       = text "keyword exceptional"
    pretty TExists            = text "keyword exists"
    pretty TFalse             = text "keyword false"
    pretty TFinally           = text "keyword finaly"
    pretty TFloat             = text "keyword float"
    pretty TForall            = text "keyword forall"
    pretty TFork              = text "keyword fork"
    pretty TIf                = text "keyword if"
    pretty TInt               = text "keyword int"
    pretty TJoin              = text "keyword join"
    pretty TLock              = text "keyword lock"
    pretty TNew               = text "keyword new"
    pretty TNull              = text "keyword null"
    pretty TRequires          = text "keyword requires"
    pretty TReturn            = text "keyword return"
    pretty TStatic            = text "keyword static"
    pretty TString            = text "keyword string"
    pretty TThrow             = text "keyword throw"
    pretty TTrue              = text "keyword true"
    pretty TTry               = text "keyword try"
    pretty TUint              = text "keyword uint"
    pretty TVoid              = text "keyword void"
    pretty TWhile             = text "keyword while"
    pretty TPlus              = text "operator '+'"
    pretty TMinus             = text "operator '-'"
    pretty TMultiply          = text "operator '*'"
    pretty TDivide            = text "operator '/'"
    pretty TModulo            = text "operator '%'"
    pretty TNot               = text "operator '!'"
    pretty TSizeOf            = text "operator '#'"
    pretty TAssign            = text "operator ':='"
    pretty TLessThan          = text "operator '<'"
    pretty TGreaterThan       = text "operator '>'"
    pretty TLessThanEqual     = text "operator '<='"
    pretty TGreaterThanEqual  = text "operator '>='"
    pretty TEqual             = text "operator '=='"
    pretty TNotEqual          = text "operator '!='"
    pretty TAnd               = text "operator '&&'"
    pretty TOr                = text "operator '||'"
    pretty TImplies           = text "operator '==>'"
    pretty TCOpen             = text "punctuator '{'"
    pretty TCClose            = text "punctuator '}'"
    pretty TSOpen             = text "punctuator '['"
    pretty TSClose            = text "punctuator ']'"
    pretty TPOpen             = text "punctuator '('"
    pretty TPClose            = text "punctuator ')'"
    pretty TDot               = text "punctuator '.'"
    pretty TComma             = text "punctuator ','"
    pretty TSemicolon         = text "punctuator ';'"
    pretty TColon             = text "punctuator ':'"
    pretty (TIntLiteral x)    = text "int literal" <+> pretty x
    pretty (TRealLiteral x)   = text "float literal" <+> pretty x
    pretty (TCharLiteral x)   = text "char literal" <+> quotes (pretty x)
    pretty (TStringLiteral x) = text "string literal" <+> doubleQuotes (pretty x)
    pretty (TIdentifier x)    = text "identifier" <+> quotes (pretty x)
}