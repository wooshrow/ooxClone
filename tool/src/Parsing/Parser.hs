module Parsing.Parser where

import           Control.Monad       (when)
import           Text.Parsec         hiding (getPosition, sourceLine, sourceColumn, Empty)
import qualified Text.Parsec         as P (getPosition)
import qualified Text.Parsec.Pos     as P (SourcePos, newPos)
import           Text.Parsec.Error  
import qualified Data.Set            as S
import           Text.Pretty
import           Parsing.Lexer
import           Language.Syntax
import           Language.Syntax.DSL
import           Data.Positioned
import           Data.Error

type P = Parsec [Positioned Token] ()

parser :: FilePath -> [Positioned Token] -> Erroneous CompilationUnit
parser fileName input =
    case parse pCompilationUnit fileName input of
        Left err     -> Left $ syntacticalError (sourcePosToPosition (errorPos err)) (getErrorMessage err)
        Right result -> Right result

getErrorMessage :: ParseError -> String
getErrorMessage = unwords . filter (not . null) . map messageString . errorMessages

pCompilationUnit :: P CompilationUnit
pCompilationUnit = do
    pos <- getPosition
    declarations <- many pClassDeclaration <* eof
    return $ CompilationUnit declarations pos

--------------------------------------------------------------------------------
-- Class parsing
--------------------------------------------------------------------------------

pClassDeclaration :: P Declaration
pClassDeclaration = do
    pos <- getPosition
    pToken TClass
    name <- pIdentifier
    members <- pBetweenCurly (many (pMember name))
    return $ Class name members pos
    
pMember :: Identifier -> P DeclarationMember
pMember className = choice [pField, pMethod className, pConstructor className]
            
pConstructor :: Identifier -> P DeclarationMember
pConstructor className = do
    pos <- getPosition
    name <- pIdentifier
    when (name /= className)
        (parserFail "Constructor identifier does not match class identifier")
    params <- pBetweenParens pParameters
    specification <- pSpecification <?> "a specification"
    body <- pBetweenCurly pStatements <?> "a statement block"
    let body' = Seq body (Return Nothing unknownLabel unknownPos) unknownLabel unknownPos
    return $ Constructor name params specification body' (unknownLabel, unknownLabel) pos

pMethod :: Identifier -> P DeclarationMember
pMethod className = do
    pos <- getPosition
    isStatic <- option False (True <$ pToken TStatic)
    ty <- pType
    name <- try pIdentifier
    when (name == className)
        (parserFail "Method name can not match class name")
    params <- pBetweenParens pParameters
    specification <- pSpecification <?> "a specification"
    body <- pBetweenCurly pStatements <?> "a statement block"
    return $ Method isStatic ty name params specification body (unknownLabel, unknownLabel) pos

pField :: P DeclarationMember
pField = do
    pos <- getPosition
    ty <- pNonVoidType
    name <- pIdentifier
    pSemicolon
    return $ Field ty name pos

pParameters :: P [Parameter]
pParameters = (pParameter `sepBy` pComma) <?> "zero or more parameters"

pParameter :: P Parameter
pParameter = do
    pos  <- getPosition
    ty   <- pNonVoidType
    name <- pIdentifier <?> "an identifier"
    return $ Parameter ty name pos

pSpecification :: P Specification
pSpecification = do
    pos <- getPosition
    requires    <- optionMaybe (pToken TRequires    *> pBetweenParens pVerificationExpression)
    ensures     <- optionMaybe (pToken TEnsures     *> pBetweenParens pVerificationExpression)
    exceptional <- optionMaybe (pToken TExceptional *> pBetweenParens pVerificationExpression)
    return $ Specification requires ensures exceptional pos

--------------------------------------------------------------------------------
-- Statement parsing
--------------------------------------------------------------------------------

pStatements :: P Statement
pStatements = do
    stat1 <- pStatement
    pos   <- getPosition
    stat2 <- optionMaybe pStatements
    return $ case stat2 of
        Just stat2' -> 
            Seq stat1 stat2' unknownLabel pos
        Nothing -> 
            case stat1 of --TODO: this can probably be removed
                Assign{_rhs = RhsCall{}} -> 
                    Seq stat1 (Skip unknownLabel pos) unknownLabel pos
                Seq{_stat2 = Assign{_rhs = RhsCall{}}} ->
                    Seq stat1 (Skip unknownLabel pos) unknownLabel pos
                _ -> 
                    stat1

pStatement :: P Statement
pStatement = choice 
    [ try pDeclare, try pAssign, pCall
    , pSkip       , pAssert    , pAssume
    , pWhile      , pIte       , pContinue
    , pBreak      , pReturn    , pThrow
    , pTry        , pBlock     , pLock
    , pJoin       , pFork ] <?> "a statement"

pDeclare :: P Statement
pDeclare = do
    pos <- getPosition
    ty <- pNonVoidType
    name <- pIdentifier
    rhs <- optionMaybe (pToken TAssign *> pRhs)
    pSemicolon
    let declaration = Declare ty name unknownLabel pos
    case rhs of
        Just rhs' -> do 
            let assign            = Assign (LhsVar name UnknownRuntimeType pos) rhs' unknownLabel pos
            let exceptionalAssign = createExceptionalItes (exceptionalRhs rhs') assign pos
            let statement         = Seq declaration exceptionalAssign unknownLabel pos
            return statement
        Nothing   -> return declaration

pAssign :: P Statement
pAssign = do
    pos <- getPosition
    lhs <- pLhs
    pToken TAssign
    rhs <- pRhs
    pSemicolon
    let statement = Assign lhs rhs unknownLabel pos
    return $ createExceptionalItes (exceptionalAssignment lhs rhs) statement pos
    
pLhs :: P Lhs
pLhs = do
    pos <- getPosition
    lhs <- choice [ try $ LhsElem  <$> pIdentifier <*> pBetweenSquare pExpression
                  , try $ LhsField <$> pIdentifier <*> pure UnknownRuntimeType <* pDot <*> pIdentifier
                  ,       LhsVar   <$> pIdentifier] <?> "a left-hand side"
    return $ lhs UnknownRuntimeType pos

pRhs :: P Rhs
pRhs = do
    pos <- getPosition
    rhs <- choice [ try pRhsCall     , try pRhsField  
                  , try pRhsNewObject, try pRhsArray
                  , try pRhsElem     , pRhsExpression ] <?> "a right-hand side"
    return $ rhs UnknownRuntimeType pos
    where
        pRhsCall       = RhsCall       <$> pInvocation
        pRhsField      = RhsField      <$> pVar <*  pDot <*> pIdentifier
        pRhsNewObject  = (\ pos name args -> RhsCall (InvokeConstructor name args Nothing unknownLabel pos)) <$> getPosition <* pToken TNew <*> pIdentifier  <*> pBetweenParens (pExpression `sepBy` pComma)
        pRhsArray      = RhsArray      <$  pToken TNew <*> pNonArrayType <*> many1 (pBetweenSquare pExpression)
        pRhsElem       = RhsElem       <$> pVar <*> pBetweenSquare pExpression
        pRhsExpression = RhsExpression <$> pExpression

pCall :: P Statement
pCall =  do
    pos <- getPosition
    invocation <- pInvocation
    return $ Call invocation unknownLabel pos

pSkip :: P Statement
pSkip = do
    pos <- getPosition
    pSemicolon
    return $ Skip unknownLabel pos

pAssert :: P Statement
pAssert = do
    pos <- getPosition
    expression <- pToken TAssert *> pVerificationExpression <* pSemicolon
    let statement = Assert expression unknownLabel pos
    return $ createExceptionalItes (exceptionalExpression expression) statement pos

pAssume :: P Statement
pAssume = do
    pos <- getPosition
    expression <- pToken TAssume *> pVerificationExpression <* pSemicolon
    let statement = Assume expression unknownLabel pos
    return $ createExceptionalItes (exceptionalExpression expression) statement pos

pWhile :: P Statement
pWhile = do
    pos <- getPosition
    guard <- pToken TWhile *> pBetweenParens pExpression
    body  <- pStatement
    let statement = createWhile guard body pos
    return $ createExceptionalItes (exceptionalExpression guard) statement pos

pIte :: P Statement
pIte = do
    startPos <- getPosition
    pToken TIf
    guard <- pBetweenParens pExpression
    trueStat <- pStatement
    elsePos <- getPosition
    falseStat <- option (Skip unknownLabel elsePos) (pToken TElse *> pStatement)
    let statement = createIte guard trueStat falseStat startPos
    return $ createExceptionalItes (exceptionalExpression guard) statement startPos

pContinue :: P Statement
pContinue = do
    pos <- getPosition
    pToken TContinue <* pSemicolon
    return $ Continue unknownLabel pos

pBreak :: P Statement
pBreak = do
    pos <- getPosition
    pToken TBreak <* pSemicolon
    return $ Break unknownLabel pos

pReturn :: P Statement
pReturn = do
    pos <- getPosition
    expression <- pToken TReturn *> optionMaybe pExpression <* pSemicolon
    let statement = Return expression unknownLabel pos
    return $ case expression of
                Just e  -> createExceptionalItes (exceptionalExpression e) statement pos
                Nothing -> statement

pThrow :: P Statement
pThrow = do
    pos <- getPosition
    pToken TThrow
    (StringLit message _) <- pStringLit
    return $ Throw message unknownLabel pos

pTry :: P Statement
pTry = do
    pos <- getPosition
    tryBody <- pToken TTry *> pBetweenCurly pStatements
    catchBody <- pToken TCatch *> pBetweenCurly pStatements
    return $ Try tryBody catchBody unknownLabel unknownLabel unknownLabel unknownLabel pos

pBlock :: P Statement
pBlock = do
    pos <- getPosition
    body <- pBetweenCurly pStatements
    return $ Block body unknownLabel pos

pLock :: P Statement
pLock = do
    lockPos <- getPosition
    pToken TLock
    name <- pBetweenParens pIdentifier
    bodyPos <- getPosition
    body <- pBetweenCurly pStatements
    unlockPos <- getPosition
    let lock   = Lock name unknownLabel lockPos
    let body'  = Block body unknownLabel bodyPos
    let unlock = Unlock name unknownLabel unlockPos
    let statement = Seq lock (Seq body' unlock unknownLabel unlockPos) unknownLabel bodyPos
    return $ createExceptionalItes (exceptionalStatement lock) statement lockPos

pJoin :: P Statement
pJoin = do
    pos <- getPosition
    pToken TJoin <* pSemicolon
    return $ Join unknownLabel pos

pFork :: P Statement
pFork = do
    pos <- getPosition
    invocation <- pToken TFork *> pInvocation <* pSemicolon
    return $ Fork invocation unknownLabel pos

pInvocation :: P Invocation
pInvocation = do
    pos <- getPosition
    lhs <- pIdentifier
    pDot
    method <- pIdentifier
    arguments <- pBetweenParens (pExpression `sepBy` pComma)
    return $ InvokeMethod lhs method arguments Nothing unknownLabel pos

--------------------------------------------------------------------------------
-- Expression parsing
--------------------------------------------------------------------------------

pExpression :: P Expression
pExpression = pExpression8 pExpression

pVerificationExpression :: P Expression
pVerificationExpression = pExpression9 pVerificationExpression

pExpression9 :: P Expression -> P Expression
pExpression9 pContinuation = pQuantifier <|> pExpression8 pContinuation
    where
        pQuantifier = do
            pos <- getPosition
            quantifier <- choice [Forall <$ pToken TForall, Exists <$ pToken TExists]
            variable   <- pIdentifier <* pComma
            index      <- pIdentifier <* pColon
            domain     <- pIdentifier <* pColon
            expression <- pExpression9 pContinuation
            return $ quantifier variable index domain expression UnknownRuntimeType pos
    
pExpression8 :: P Expression -> P Expression
pExpression8 pContinuation = pExpression7 pContinuation `chainl1` pOperators
    where
        pOperators = do
            pos <- getPosition
            (\ e1 e2 -> BinOp Implies e1 e2 UnknownRuntimeType pos) <$ pToken TImplies

pExpression7 :: P Expression -> P Expression
pExpression7 pContinuation = pExpression6 pContinuation `chainl1` pOperators
    where
        pOperators = do
            pos <- getPosition
            op <- choice [ BinOp And <$ pToken TAnd
                         , BinOp Or  <$ pToken TOr ]
            return (\ e1 e2 -> op e1 e2 UnknownRuntimeType pos)

pExpression6 :: P Expression -> P Expression
pExpression6 pContinuation = pExpression5 pContinuation `chainl1` pOperators
    where
        pOperators = do
            pos <- getPosition
            op <- choice [ BinOp Equal    <$ pToken TEqual    
                         , BinOp NotEqual <$ pToken TNotEqual ]
            return (\ e1 e2 -> op e1 e2 UnknownRuntimeType pos)

pExpression5 :: P Expression -> P Expression
pExpression5 pContinuation = pExpression4 pContinuation `chainl1` pOperators
    where
        pOperators =  do
            pos <- getPosition
            op <- choice [ BinOp LessThan         <$ pToken TLessThan        
                         , BinOp LessThanEqual    <$ pToken TLessThanEqual   
                         , BinOp GreaterThan      <$ pToken TGreaterThan     
                         , BinOp GreaterThanEqual <$ pToken TGreaterThanEqual ]
            return (\ e1 e2 -> op e1 e2 UnknownRuntimeType pos)

pExpression4 :: P Expression -> P Expression
pExpression4 pContinuation = pExpression3 pContinuation `chainl1` pOperators
    where
        pOperators = do
            pos <- getPosition
            op <- choice [ BinOp Plus  <$ pToken TPlus 
                         , BinOp Minus <$ pToken TMinus ]
            return (\ e1 e2 -> op e1 e2 UnknownRuntimeType pos)

pExpression3 :: P Expression -> P Expression
pExpression3 pContinuation = pExpression2 pContinuation `chainl1` pOperators
    where
        pOperators =  do
            pos <- getPosition
            op <- choice [ BinOp Multiply <$ pToken TMultiply
                         , BinOp Divide   <$ pToken TDivide   
                         , BinOp Modulo   <$ pToken TModulo ]
            return (\ e1 e2 -> op e1 e2 UnknownRuntimeType pos)

pExpression2 :: P Expression -> P Expression
pExpression2 pContinuation = pExpression1 pContinuation `chainlUnary1` pOperators
    where
        pOperators = do
            pos <- getPosition
            op  <- choice [ UnOp Negative <$ pToken TMinus
                          , UnOp Negate   <$ pToken TNot ]
            return (\ e -> op e UnknownRuntimeType pos)

pExpression1 :: P Expression -> P Expression
pExpression1 pContinuation = do
    pos <- getPosition
    choice [ pVar
           , Lit    <$> pLit <*> pure UnknownRuntimeType <*> pure pos
           , SizeOf <$  pToken TSizeOf <*> pIdentifier <*> pure UnknownRuntimeType <*> pure pos
           , pBetweenParens pContinuation]

pVar :: P Expression
pVar = do 
    pos <- getPosition
    var <- pIdentifier
    return $ Var var UnknownRuntimeType pos

pLit :: P Lit
pLit = choice
    [ pBoolLit  , pIntLit , pRealLit
    , pStringLit, pCharLit, pNullLit ] <?> "a literal"

pBoolLit :: P Lit
pBoolLit = do
    pos <- getPosition
    choice [ BoolLit True  pos <$ pToken TTrue
           , BoolLit False pos <$ pToken TFalse ] <?> "a boolean literal"

pIntLit :: P Lit
pIntLit = do
    sourceFileName <- getSourceName
    token showToken (posFromToken sourceFileName) (matchToken sourceFileName)
    where
        showToken (Positioned _ tok') = toString tok'
        matchToken name (Positioned pos (TIntLiteral value)) = intLiteral value (pos{sourceFile=name})
        matchToken _    _                                    = Nothing
        intLiteral value pos = Just $ IntLit value pos
          --  | value >= 0       && value < (2^32) = Just $ UIntLit value
          --  | value >= -(2^31) && value < (2^31) = Just $ IntLit  value
          --  | otherwise                          = Nothing

pRealLit :: P Lit
pRealLit = do
    sourceFileName <- getSourceName
    token showToken (posFromToken sourceFileName) matchToken
    where
        showToken (Positioned _ tok') = toString tok'
        matchToken (Positioned pos (TRealLiteral value)) = Just $ FloatLit value pos
        matchToken _                                     = Nothing

pStringLit :: P Lit
pStringLit = do
    sourceFileName <- getSourceName
    token showToken (posFromToken sourceFileName) matchToken
    where
        showToken (Positioned _ tok') = toString tok'
        matchToken (Positioned pos (TStringLiteral value)) = Just $ StringLit value pos
        matchToken _                                       = Nothing

pCharLit :: P Lit
pCharLit = do
    sourceFileName <- getSourceName
    token showToken (posFromToken sourceFileName) matchToken
    where
        showToken (Positioned _ tok') = toString tok'
        matchToken (Positioned pos (TCharLiteral value)) = Just $ CharLit value pos
        matchToken _                                     = Nothing

pNullLit :: P Lit
pNullLit = do
    pos <- getPosition
    NullLit <$ pToken TNull <*> pure pos <?> "null"

--------------------------------------------------------------------------------
-- Type parsing
--------------------------------------------------------------------------------

pType :: P Type
pType = do
    pos <- getPosition    
    ty <- choice [ Nothing <$  pToken TVoid
                 , Just    <$> pNonVoidType ] <?> "a type or void"
    return $ Type ty pos

pNonArrayType :: P NonVoidType
pNonArrayType = do
    pos <- getPosition
    choice [ UIntType      <$  pToken TUint , IntType    <$ pToken TInt
           , FloatType     <$  pToken TFloat, StringType <$ pToken TString
           , BoolType      <$  pToken TBool , CharType   <$ pToken TChar
           , ReferenceType <$> pIdentifier  ] <*> pure pos <?> "a type"

pNonVoidType :: P NonVoidType
pNonVoidType = do
    pos <- getPosition
    baseTy <- choice 
        [ UIntType      <$  pToken TUint , IntType    <$ pToken TInt
        , FloatType     <$  pToken TFloat, StringType <$ pToken TString
        , BoolType      <$  pToken TBool , CharType   <$ pToken TChar
        , ReferenceType <$> pIdentifier  ] <?> "a type"
    ty <- pArrayRanks 
    return $ ty (baseTy pos)

pArrayRanks :: P (NonVoidType -> NonVoidType)
pArrayRanks = do
    pos <- getPosition
    isArrayTy <- optionMaybe (ArrayType <$ pSOpen <* pSClose)
    case isArrayTy of
        Just arrayTy -> do 
            nextArrayTy <- pArrayRanks
            return (\ innerTy -> arrayTy (nextArrayTy innerTy) pos)
        Nothing      -> return id

--------------------------------------------------------------------------------
-- Token parsing
--------------------------------------------------------------------------------

pToken :: Token -> P ()
pToken tok = do
    sourceFileName <- getSourceName
    token showToken (posFromToken sourceFileName) matchToken <?> toString tok
    where
        showToken (Positioned _ tok') = toString tok'
        matchToken (Positioned _ tok') = if tok == tok' then Just () else Nothing

pSemicolon :: P ()
pSemicolon = pToken TSemicolon

pColon :: P ()
pColon = pToken TColon

pComma :: P ()
pComma = pToken TComma

pDot :: P ()
pDot = pToken TDot

--------------------------------------------------------------------------------
-- Auxiliary functions
--------------------------------------------------------------------------------

pIdentifier :: P Identifier
pIdentifier = do
    pos <- getPosition
    sourceFileName <- getSourceName
    Identifier <$> token showToken (posFromToken sourceFileName) matchToken <*> pure pos
    where
        showToken (Positioned _ tok') = toString tok'
        matchToken (Positioned _ (TIdentifier s)) = Just s
        matchToken _                              = Nothing

pBetweenCurly :: P a -> P a
pBetweenCurly = between (pToken TCOpen) (pToken TCClose)

pBetweenParens :: P a -> P a
pBetweenParens = between (pToken TPOpen) (pToken TPClose)

pSOpen, pSClose :: P ()
pSOpen  = pToken TSOpen
pSClose = pToken TSClose

pBetweenSquare :: P a -> P a
pBetweenSquare = between pSOpen pSClose

posFromToken :: SourceName -> Positioned a -> P.SourcePos
posFromToken sourceFileName (Positioned pos _) 
    = P.newPos sourceFileName (sourceLine pos) (sourceColumn pos)

getSourceName :: P SourceName
getSourceName = sourceFile <$> getPosition

getPosition :: P Position
getPosition = sourcePosToPosition <$> P.getPosition

chainlUnary1 :: P a -> P (a -> a) -> P a
chainlUnary1 p op = do
    fs <- many op
    x  <- p
    return $ foldr id x fs

--------------------------------------------------------------------------------
-- Exception Statement Creation
--------------------------------------------------------------------------------

exceptionalStatement :: Statement -> S.Set Expression
exceptionalStatement Lock{_var} 
    = S.singleton $ var' _var REFRuntimeType `equal'` lit' nullLit' 
--exceptionalStatement Unlock{_var} 
--    = Just $ var' _var REFRuntimeType `equal'` lit' nullLit' 
exceptionalStatement Assign{_lhs,_rhs}
    = exceptionalAssignment _lhs _rhs
exceptionalStatement _ 
    = S.empty

exceptionalAssignment :: Lhs -> Rhs -> S.Set Expression
exceptionalAssignment lhs rhs = exceptionalLhs lhs `S.union` exceptionalRhs rhs
        
exceptionalLhs :: Lhs -> S.Set Expression
exceptionalLhs LhsVar{}     = S.empty
exceptionalLhs LhsField{..} = S.singleton (var' _var _ty `equal'` lit' nullLit')
exceptionalLhs LhsElem{..}  = S.fromList [ var' _var _ty `equal'` lit' nullLit'
                                         , ors' [ _index `greaterthanequal'` sizeof' _var
                                                , _index `lessthan'` lit' (intLit' 0) ] ]
                                        
exceptionalRhs :: Rhs -> S.Set Expression
exceptionalRhs RhsExpression{_value}
    = exceptionalExpression _value
exceptionalRhs RhsField{_var}
    = S.singleton (_var `equal'` lit' nullLit')
exceptionalRhs RhsElem{_var, _index}
    = let (Var var _ _) = _var
       in S.fromList [ _var   `equal'` lit' nullLit'
                     , ors' [ _index `lessthan'` lit' (intLit' 0)
                            , _index `greaterthanequal'` sizeof' var ] ]
exceptionalRhs RhsCall{_invocation}
    = exceptionalInvocation _invocation
exceptionalRhs RhsArray{_sizes}
    = (S.singleton . ors' . map (`lessthanequal'` lit' (intLit' 0))) _sizes

exceptionalInvocation :: Invocation -> S.Set Expression
exceptionalInvocation invocation 
    = if null (nonNullLhs `S.union` exceptionalArgs)
        then S.empty
        else S.singleton (ors' (nonNullLhs `S.union` exceptionalArgs))
    where
        exceptionalArgs = S.unions (map exceptionalExpression (_arguments invocation))
        nonNullLhs -- if is non-static a method, lhs must be non-null
            | Just (_, member)   <- _resolved invocation
            , InvokeMethod{_lhs} <- invocation
            , Method{_isStatic}  <- member
            , not _isStatic
                = S.singleton (var' _lhs REFRuntimeType `equal'` lit' nullLit')
            | otherwise
                = S.empty

exceptionalExpression :: Expression -> S.Set Expression
exceptionalExpression expression 
    | null exceptions = S.empty
    | otherwise       = S.singleton (ors' exceptions)
    where
        exceptions = exceptional' expression
        exceptional' expression' 
            = case expression' of
                Forall{..} -> S.singleton (var' _domain BoolRuntimeType `equal'` lit' nullLit') `S.union` exceptional' _formula
                Exists{..} -> S.singleton (var' _domain BoolRuntimeType `equal'` lit' nullLit') `S.union` exceptional' _formula
                BinOp{..}  -> 
                    let condition = _rhs `equal'` lit' (IntLit 0 (getPos expression'))
                        in (case _binOp of
                            Divide -> S.singleton condition
                            Modulo -> S.singleton condition
                            _      -> S.empty)
                                `S.union` exceptional' _lhs `S.union` exceptional' _rhs
                UnOp{_value}    -> exceptional' _value
                Var{}           -> S.empty
                SymbolicVar{}   -> S.empty
                Lit{}           -> S.empty
                SizeOf{_var}    -> S.singleton (var' _var ARRAYRuntimeType `equal'` lit' nullLit')
                Ref{}           -> S.empty
                SymbolicRef{}   -> S.empty
                Conditional{..} -> exceptional' _guard `S.union` exceptional' _true `S.union` exceptional' _false

--------------------------------------------------------------------------------
-- Statement Creation
--------------------------------------------------------------------------------

createExceptionalItes :: S.Set Expression -> Statement -> Position -> Statement
createExceptionalItes exceptions body pos
    = S.foldr ( \ exception stat -> createExceptionalIte exception stat pos) body exceptions

createExceptionalIte :: Expression -> Statement -> Position -> Statement
createExceptionalIte guard stat pos = createIte guard (Throw "exception" unknownLabel pos) stat pos

createIte :: Expression -> Statement -> Statement -> Position -> Statement
createIte guard trueS falseS pos
    = let trueBranch  = Block (Seq (Assume guard unknownLabel pos) trueS unknownLabel pos) unknownLabel pos
          falseBranch = Block (Seq (Assume (neg' guard) unknownLabel pos) falseS unknownLabel pos) unknownLabel pos
       in Ite guard trueBranch falseBranch unknownLabel pos

createWhile :: Expression -> Statement -> Position -> Statement
createWhile guard body pos
    = let body' = Block (Seq (Assume guard unknownLabel pos) body unknownLabel pos) unknownLabel pos
       in Seq (While guard body' unknownLabel pos) (Assume (neg' guard) unknownLabel pos) unknownLabel pos