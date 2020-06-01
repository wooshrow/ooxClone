module Analysis.Type.Analysis where

import           Polysemy
import           Polysemy.State
import           Polysemy.Reader
import           Polysemy.Error              (Error, throw)
import           Control.Monad
import qualified Data.Map               as M
import qualified Data.Set               as S
import           Data.Foldable               (find)
import           Data.Maybe                  (fromJust)
import           Control.Lens
import           Control.Lens.Extras
import           Text.Pretty
import           Syntax.Syntax
import           Syntax.Fold
import           Syntax.DSL
import qualified Syntax.Lenses          as SL
import           Analysis.SymbolTable
import           Analysis.Type.Typeable
import           Data.Error
import           Data.Positioned

--------------------------------------------------------------------------------
-- Type Environment
--------------------------------------------------------------------------------

type TypingEffects r a = Members [Reader SymbolTable, State TypeEnvironment, Error ErrorMessage] r => Sem r a

type TypeEnvironment = M.Map Identifier RuntimeType

inNewScope :: TypingEffects r a -> TypingEffects r a
inNewScope f = do
    state <- get
    result <- f
    put state
    return result

declareParam :: Parameter -> TypingEffects r ()
declareParam param = declareVar (param ^?! SL.name) (typeOf (param ^?! SL.ty))

declareVar :: Identifier -> RuntimeType -> TypingEffects r ()
declareVar var ty = do
    env <- get
    case env M.!? var of
        Just _ -> do
            let old = (fromJust . find (var ==) . map fst . M.toList) env
            throw (shadowError var old)
        Nothing -> modify (M.insert var ty)

getVarType :: (Identifier -> ErrorMessage) -> Identifier -> TypingEffects r RuntimeType
getVarType err var = do
    env <- get
    maybe (throw (err var)) return (env M.!? var)

matchMaybeType :: (WithPos a, Typeable a, Typeable b) => Maybe a -> b -> TypingEffects r ()
matchMaybeType a b = maybe (return ()) (`matchType` b) a

matchType :: (WithPos a, Typeable a, Typeable b) => a -> b -> TypingEffects r ()
matchType a b = unless (a `isOfType` b) 
                    (throw (unificationError (getPos a) (typeOf b) (typeOf a)))

--------------------------------------------------------------------------------
-- Typing
--------------------------------------------------------------------------------

typeCompilationUnit :: CompilationUnit -> TypingEffects r CompilationUnit
typeCompilationUnit program = do
    members' <- mapM typeDeclaration (program ^. SL.members)
    return $ program & (SL.members .~ members')

typeDeclaration :: Declaration -> TypingEffects r Declaration
typeDeclaration declaration = do
    members' <- mapM (typeMember declaration) (declaration ^?! SL.members)
    return $ declaration & (SL.members .~ members')

typeMember :: Declaration -> DeclarationMember -> TypingEffects r DeclarationMember
typeMember _ field@Field{} = return field
typeMember currentClass method = inNewScope $ do
    mapM_ declareParam (method ^?! SL.params)
    when (SL._Constructor `is` method || not (method ^?! SL.isStatic)) (do
        let name = Identifier "this" (getPos (currentClass ^. SL.name))
        let ty   = ReferenceRuntimeType (currentClass ^?! SL.name)
        declareVar name ty)
    specification' <- typeSpecification method (method ^?! SL.specification)
    body' <- typeStatement (currentClass, method) (method ^?! SL.body)
    return $ method & (SL.specification .~ specification') & (SL.body .~ body')

typeSpecification :: DeclarationMember -> Specification -> TypingEffects r Specification
typeSpecification currentMethod specification = do
    requires' <- typeMaybeExpression (specification ^. SL.requires)
    matchMaybeType requires' BoolRuntimeType
    ensures' <- inNewScope (do
        unless (currentMethod `isOfType` VoidRuntimeType)
            (declareVar (Identifier "retval" unknownPos) (typeOf currentMethod))
        typeMaybeExpression (specification ^. SL.ensures))
    matchMaybeType ensures' BoolRuntimeType
    exceptional' <- typeMaybeExpression (specification ^. SL.exceptional)
    matchMaybeType exceptional' BoolRuntimeType
    return $ specification & (SL.requires .~ requires') 
                           & (SL.ensures .~ ensures')
                           & (SL.exceptional .~ exceptional')

typeStatement :: (Declaration, DeclarationMember) -> Statement -> TypingEffects r Statement
typeStatement current = foldStatement (statementAlgebra current)

statementAlgebra :: Members [Reader SymbolTable, State TypeEnvironment, Error ErrorMessage] r => 
    (Declaration, DeclarationMember) -> StatementAlgebra (Sem r Statement)
statementAlgebra (_, currentMethod) =
    ( -- Declare
      \ ty name l pos -> do
        declareVar name (typeOf ty)
        return $ Declare ty name l pos
      -- Assign
    , \ lhs rhs l pos -> do
        lhs' <- typeLhs lhs
        rhs' <- typeRhs lhs' rhs
        matchType rhs' lhs'
        return $ Assign lhs' rhs' l pos
      -- Call
    , \ inv l pos -> do
        inv' <- typeInvocation ANYRuntimeType inv
        return $ Call inv' l pos
      -- Skip
    , \ l pos -> return $ Skip l pos
      -- Assert
    , \ expr l pos -> do
        expr' <- typeExpression expr
        matchType expr' BoolRuntimeType
        return $ Assert expr' l pos
      -- Assume
    , \ expr l pos -> do
        expr' <- typeExpression expr
        matchType expr' BoolRuntimeType
        return $ Assume expr' l pos
      -- While
    , \ guard body l pos -> do
        guard' <- typeExpression guard
        matchType guard' BoolRuntimeType
        body' <- inNewScope body
        return $ While guard' body' l pos
      -- Ite
    , \ guard tBody fBody l pos -> do
        guard' <- typeExpression guard
        matchType guard' BoolRuntimeType
        tBody' <- inNewScope tBody
        fBody' <- inNewScope fBody
        return $ Ite guard' tBody' fBody' l pos
      -- Continue
    , \ l pos -> return $ Continue l pos
      -- Break
    , \ l pos -> return $ Break l pos
      -- Return
    , \ expr l pos -> do
        expr' <- typeMaybeExpression expr
        if SL._Constructor `is` currentMethod
            then
                case expr' of
                    Just expr'' ->
                        throw (unexpectedReturnValueError expr'')
                    Nothing -> do
                        let thisVar = var' (Identifier "this" unknownPos) (typeOf currentMethod)
                        return $ Return (Just thisVar) l pos
            else 
                case expr' of
                    Just expr'' -> do
                        matchType expr'' currentMethod
                        return $ Return (Just expr'') l pos
                    Nothing -> do
                        unless (VoidRuntimeType `isOfType` currentMethod)
                            (throw (expectedReturnValueError (typeOf currentMethod) pos))
                        return $ Return Nothing l pos
      -- Throw
    , \ m l pos -> return $ Throw m l pos
      -- Try
    , \ tBody cBody l1 l2 l3 l4 pos -> do
        tBody' <- inNewScope tBody
        cBody' <- inNewScope cBody
        return $ Try tBody' cBody' l1 l2 l3 l4 pos
      -- Block
    , \ body l pos -> Block <$> inNewScope body <*> pure l <*> pure pos
      -- Lock
    , \ var l pos -> do
        ty <- getVarType readOfUndeclaredVarError var
        matchType (Var var ty (getPos var)) REFRuntimeType
        return $ Lock var l pos
        -- Unlock
    , \ var l pos -> do
        ty <- getVarType readOfUndeclaredVarError var
        matchType (Var var ty (getPos var)) REFRuntimeType
        return $ Unlock var l pos
      -- Fork
    , \ inv l pos -> do
        inv' <- typeInvocation ANYRuntimeType inv
        return $ Fork inv' l pos
      -- Join
    , \ l pos -> return $ Join l pos
      -- Seq
    , \ s1 s2 l pos -> Seq <$> s1 <*> s2 <*> pure l <*> pure pos)

typeLhs :: Lhs -> TypingEffects r Lhs
typeLhs (LhsVar name _ pos) = do
     ty <- getVarType writeToUndeclaredVarError name
     return $ LhsVar name ty pos
typeLhs (LhsField var _ field _ pos) = do
    table <- ask
    ty <- getVarType writeToUndeclaredVarError var
    unless (SL._ReferenceRuntimeType `is` ty)
        (throw (unificationError (getPos var) REFRuntimeType ty))
    let (ReferenceRuntimeType className) = ty
    let symbol = lookupField className field table
    case symbol of
        Nothing    -> throw (unresolvedFieldError className field)
        Just entry -> do
            let (Field fieldTy _ _) = getMember entry
            return $ LhsField var ty field (typeOf fieldTy) pos
typeLhs (LhsElem var index _ pos) = do
    ty <- getVarType writeToUndeclaredVarError var
    matchType (Positioned (getPos var) ty) ARRAYRuntimeType
    let elemTy = ty ^?! SL.innerTy
    index' <- typeExpression index
    matchType index' IntRuntimeType
    return $ LhsElem var index' elemTy pos 

typeRhs :: Lhs -> Rhs -> TypingEffects r Rhs
typeRhs _ (RhsExpression expr _ pos) = do
    expr' <- typeExpression expr
    let ty = typeOf expr'
    return $ RhsExpression expr' ty pos
typeRhs _ (RhsField var field _ pos) = do
    table <- ask
    var' <- typeExpression var
    unless (SL._ReferenceRuntimeType `is` typeOf var')
        (throw (unificationError (getPos var') REFRuntimeType (typeOf var')))
    let (ReferenceRuntimeType className) = typeOf var'
    let symbol = lookupField className field table
    case symbol of
        Nothing    -> throw (unresolvedFieldError className field)
        Just entry -> do
            let (Field fieldTy _ _) = getMember entry
            return $ RhsField var' field (typeOf fieldTy) pos
typeRhs _ (RhsElem var index _ pos) = do
    var' <- typeExpression var
    matchType var' ARRAYRuntimeType
    let elemTy = typeOf var' ^?! SL.innerTy
    index' <- typeExpression index
    matchType index' IntRuntimeType
    return $ RhsElem var' index' elemTy pos
typeRhs lhs (RhsCall inv _ pos) = do
    inv' <- typeInvocation (typeOf lhs) inv
    let ty = typeOf inv'
    return $ RhsCall inv' ty pos
typeRhs _ (RhsArray ty sizes _ pos) = do
    sizes' <- mapM typeExpression sizes
    mapM_ (`matchType` IntRuntimeType) sizes'
    let arrayTy = foldr (\ _ b -> ArrayRuntimeType b) (typeOf ty) sizes
    return $ RhsArray ty sizes' arrayTy pos

typeInvocation :: RuntimeType -> Invocation -> TypingEffects r Invocation
typeInvocation targetTy (InvokeMethod lhs methodName arguments _ label pos) = do
    table <- ask
    arguments' <- mapM typeExpression arguments
    env <- get
    let isVariableTy = env M.!? lhs
    let argTypes     = maybe [] pure isVariableTy ++ map typeOf arguments'
    let className    = maybe lhs (\ ty -> Identifier (toString ty) (getPos lhs)) isVariableTy
    let methods      = S.toList <$> matchMethod className methodName targetTy argTypes table
    case methods of
        Just [symbol] -> return $ InvokeMethod lhs methodName arguments' (Just (getDeclaration symbol, getMember symbol)) label pos
        Just _        -> throw (duplicateResolvedMethodError className methodName targetTy argTypes)
        Nothing       -> throw (unresolvedMethodError className methodName targetTy argTypes)
typeInvocation targetTy inv@InvokeConstructor{} = do
    table <- ask
    arguments' <- mapM typeExpression (inv ^. SL.arguments)
    let className = inv ^?! SL.className
    let argTypes  = map typeOf arguments'
    let methods   = S.toList <$> matchMethod className className targetTy argTypes table
    case methods of
        Just [symbol] -> return $ inv & (SL.arguments .~ arguments') & SL.resolved ?~ (getDeclaration symbol, getMember symbol)
        Just _        -> throw (duplicateResolvedMethodError className className targetTy argTypes)
        Nothing       -> throw (unresolvedMethodError className className targetTy argTypes)

typeMaybeExpression :: Maybe Expression -> TypingEffects r (Maybe Expression)
typeMaybeExpression = maybe (return Nothing) (fmap Just . typeExpression)

typeExpression :: Expression -> TypingEffects r Expression
typeExpression = foldExpression expressionAlgebra

expressionAlgebra :: Members [Reader SymbolTable, State TypeEnvironment, Error ErrorMessage] r => ExpressionAlgebra (Sem r Expression)
expressionAlgebra = ExpressionAlgebra 
    { fForall = \ elem range domain formula _ pos -> do
        arrayTy <- getVarType readOfUndeclaredVarError domain
        matchType (Var domain arrayTy (getPos domain)) ARRAYRuntimeType
        let ArrayRuntimeType innerArrayTy = arrayTy
        formula' <- inNewScope (do
            declareVar elem innerArrayTy
            declareVar range IntRuntimeType
            formula)
        matchType formula' BoolRuntimeType
        return $ Forall elem range domain formula' BoolRuntimeType pos

    , fExists = \ elem range domain formula _ pos -> do
        arrayTy <- getVarType readOfUndeclaredVarError domain
        matchType (Var domain arrayTy (getPos domain)) ARRAYRuntimeType
        let ArrayRuntimeType innerArrayTy = arrayTy
        formula' <- inNewScope (do
            declareVar elem innerArrayTy
            declareVar range IntRuntimeType
            formula)
        matchType formula' BoolRuntimeType
        return $ Exists elem range domain formula' BoolRuntimeType pos
    
    , fBinOp = \ binOp lhs rhs _ pos -> do
        lhs' <- lhs
        rhs' <- rhs
        ty <- typeBinOp binOp lhs' rhs'
        return $ BinOp binOp lhs' rhs' ty pos

    , fUnOp = \ unOp value _ pos -> do
        value' <- value
        ty <- case unOp of
            Negative -> matchType value' NUMRuntimeType >> return (typeOf value')
            Negate   -> matchType value' BoolRuntimeType >> return BoolRuntimeType
        return $ UnOp unOp value' ty pos

    , fVar = \ name _ pos -> do
        ty <- getVarType readOfUndeclaredVarError name
        return $ Var name ty pos

    , fSymVar = error "Symbolic variable in analysis phase"

    , fLit = \ lit _ pos -> return $ Lit lit (typeOf lit) pos
    
    , fSizeOf = \ var _ pos -> return $ SizeOf var IntRuntimeType pos
    
    , fRef = \ ref _ pos -> return $ Ref ref REFRuntimeType pos
    
    , fSymRef = error "Symbolic reference in analysis phase"

    , fIte = error "Ite in analysis phase" }

typeBinOp :: BinOp -> Expression -> Expression -> TypingEffects r RuntimeType
typeBinOp op exp1 exp2
    | op `elem` [Implies, And, Or]
        = matchType exp1 BoolRuntimeType >> matchType exp2 BoolRuntimeType >> return BoolRuntimeType
    | op `elem` [Equal, NotEqual]
        = matchType exp2 exp1 >> return BoolRuntimeType
    | op `elem` [LessThan, LessThanEqual, GreaterThan, GreaterThanEqual]
        = matchType exp1 NUMRuntimeType >> matchType exp2 NUMRuntimeType >> return BoolRuntimeType
    | op `elem` [Plus, Minus, Multiply, Divide, Modulo]
        = matchType exp1 NUMRuntimeType >> matchType exp2 NUMRuntimeType >> return (typeOf exp1)
    | otherwise 
        = error "typeBinOp - missing case"
