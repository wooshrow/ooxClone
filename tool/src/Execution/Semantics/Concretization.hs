module Execution.Semantics.Concretization(
      Concretization
    , concretizes
    , concretize
    , concretizeWithResult
    , concretesOfTypeM
    , concretesOfType
    , initializeSymbolicRef
    , createSymbolicVar
) where

import qualified Data.Map as M
import qualified Data.Set as S
import           Data.Maybe
import           Data.Configuration
import           Data.Positioned
import           Control.Monad
import           Control.Monad.Extra
import           Text.Pretty
import           Control.Lens hiding (assign)
import           Polysemy.Reader
import           Polysemy.Error
import           Language.Syntax
import           Language.Syntax.Fold
import           Language.Syntax.DSL
import qualified Language.Syntax.Lenses as SL
import           Analysis.Type.Typeable
import           Analysis.SymbolTable
import           Execution.State
import           Execution.State.Heap
import           Execution.State.AliasMap as AliasMap
import           Execution.Semantics.Heap
import           Execution.Semantics.StackFrame
import           Verification.Result

--------------------------------------------------------------------------------
-- Concretization
--------------------------------------------------------------------------------

type Concretization = M.Map Identifier Expression

concretizes :: [Concretization] -> ExecutionState -> (ExecutionState -> Engine r [ExecutionState]) -> Engine r [ExecutionState]
concretizes [] state f = f state
concretizes cs state f = concatForM cs $ \ concretization ->
    f $ foldr (\ (symRef, concRef) stateN -> stateN & (aliasMap %~ AliasMap.insert symRef (S.singleton concRef))) state (M.toList concretization)

concretize  :: [Concretization] -> ExecutionState -> (ExecutionState -> Engine r ExecutionState) -> Engine r [ExecutionState]
concretize [] state f = (:[]) <$> f state
concretize cs state f = forM cs $ \ concretization ->
    f $ foldr (\ (symRef, concRef) stateN -> stateN & (aliasMap %~ AliasMap.insert symRef (S.singleton concRef))) state (M.toList concretization)

concretizeWithResult :: [Concretization] -> ExecutionState -> (ExecutionState -> Engine r (ExecutionState, a)) -> Engine r [(ExecutionState, a)]
concretizeWithResult [] state f = (:[]) <$> f state
concretizeWithResult cs state f = forM cs $ \ concretization ->
    f $ foldr (\ (symRef, concRef) stateN -> stateN & (aliasMap %~ AliasMap.insert symRef (S.singleton concRef))) state (M.toList concretization)

concretesOfTypeM :: ExecutionState -> RuntimeType -> Maybe Expression -> Engine r (ExecutionState, [Concretization])
concretesOfTypeM state ty = maybe (return (state, [])) (concretesOfType state ty)

concretesOfType :: ExecutionState -> RuntimeType -> Expression -> Engine r (ExecutionState, [Concretization])
concretesOfType state0 ty formula
    | Just _ <- getCurrentThread state0 = do
        refs <- findSymbolicRefsOfType state0 ty formula
        if null refs
            then 
                return (state0, [])
            else do
                state1 <- foldM initializeSymbolicRef state0 refs
                let mappings = map (\ ref -> map (ref ^?! SL.var, ) (S.toList (fromMaybe (error "concretesOfType") (AliasMap.lookup (ref ^?! SL.var) (state1 ^. aliasMap))))) (S.toList refs)
                return (state1, map M.fromList (sequence mappings))
    | otherwise =
        throw (InternalError "concretesOfType: cannot get current thread")

findSymbolicRefsOfType :: ExecutionState -> RuntimeType -> Expression -> Engine r (S.Set Expression)
findSymbolicRefsOfType state ty = foldExpression algebra
    where
        algebra = monoidMExpressionAlgebra
            { {- fForall = \ _ _ domain formula _ _ -> do
                ref <- readDeclaration state domain
                inners <- if ref `isOfType` ty
                    then processRef ref 
                            (return . const S.empty) 
                            (return . S.singleton) 
                            (return S.empty)
                    else return S.empty
                S.union inners <$> formula

            , fExists = \ _ _ domain formula _ _ -> do
                ref <- readVar thread domain
                inners <- if ref `isOfType` ty
                    then processRef ref 
                            (return . const S.empty) 
                            (return . S.singleton) 
                            (return S.empty)
                    else return S.empty
                S.union inners <$> formula
            
            ,-} fSizeOf = \ var _ _ -> do
                ref <- readDeclaration state var
                case ref of
                    Lit NullLit{} _ _     -> return S.empty
                    Ref{}                 -> return S.empty
                    SymbolicRef _ varTy _ -> return $ if varTy `isOfType` ty 
                        then S.singleton ref
                        else S.empty
                        
            , fSymRef = \ symVar varTy varPos -> return $ if varTy `isOfType` ty 
                            then S.singleton (SymbolicRef symVar varTy varPos) 
                            else S.empty }

--------------------------------------------------------------------------------
-- Lazy Symbolic Reference Initialization
--------------------------------------------------------------------------------

initializeSymbolicRef :: ExecutionState -> Expression -> Engine r ExecutionState
initializeSymbolicRef state var@(SymbolicRef ref ty _)
    | AliasMap.member ref (state ^. aliasMap) = 
        return state
    | ty `isOfType` ARRAYRuntimeType = do
        debug ("Initializing symbolic reference '" ++ toString var ++ ":" ++ toString ty ++ "' ")
        initializeSymbolicArrays state var
    | otherwise = do
        debug ("Initializing symbolic reference '" ++ toString var ++ ":" ++ toString ty ++ "' ")
        initializeSymbolicObject state var
            
createSymbolicVar :: Typeable a => Identifier -> a -> Expression
createSymbolicVar (Identifier name pos) ty
    | ty `isOfType` REFRuntimeType = SymbolicRef (Identifier ('_' : name) pos) (typeOf ty) pos
    | otherwise                    = SymbolicVar (Identifier ('_' : name) pos) (typeOf ty) pos

--------------------------------------------------------------------------------
-- Lazy Symbolic Array Initialization

initializeSymbolicArrays :: ExecutionState -> Expression -> Engine r ExecutionState
initializeSymbolicArrays state0 var@(SymbolicRef ref ty _) = do
    config <- askConfig
    let sizes = [1..symbolicArraySize config]
    (state1, refs) <- mapAccumM ( \ stateN -> initializeSymbolicArray stateN var) state0 sizes
    let aliases  = if symbolicAliases config then otherAliasesOfType ty (state1 ^. aliasMap) else S.empty
    let nullCase = if symbolicNulls config then S.singleton (lit' nullLit') else S.empty
    let cases    = S.fromList refs `S.union` aliases `S.union` nullCase
    return $ state1 & (aliasMap %~ AliasMap.insert ref cases)

initializeSymbolicArray :: ExecutionState -> Expression -> Int -> Engine r (ExecutionState, Expression)
initializeSymbolicArray state (SymbolicRef ref ty _) size = do
    let elemTy  = ty ^?! SL.innerTy    
    let indices = [0..size - 1]
    structure <- ArrayValue <$> mapM (initializeSymbolicElem state elemTy) indices
    allocate state structure

initializeSymbolicElem :: ExecutionState -> RuntimeType -> Int -> Engine r Expression
initializeSymbolicElem state ty index = do
    let symNameIndex = size (state ^. heap) + 1
    let symName      = Identifier (show symNameIndex ++ show index) unknownPos
    return $ createSymbolicVar symName ty

--------------------------------------------------------------------------------
-- Lazy Symbolic Object Initialization

initializeSymbolicObject :: ExecutionState -> Expression -> Engine r ExecutionState
initializeSymbolicObject state0 (SymbolicRef ref ty _) = do
    (config, _, table) <- ask
    let fields = (S.toList . S.map getMember . getAllFields (ty ^?! SL.ty)) table
    values <- mapM (initializeSymbolicField state0) fields
    let structure = ObjectValue (M.fromList values) ty
    (state1, concRef) <- allocate state0 structure
    let aliases  = if symbolicAliases config then otherAliasesOfType ty (state1 ^. aliasMap) else S.empty
    let nullCase = if symbolicNulls config then S.singleton (lit' nullLit') else S.empty
    let cases    = S.insert concRef (aliases `S.union` nullCase)
    return $ state1 & (aliasMap %~ AliasMap.insert ref cases)

initializeSymbolicObject _ expression =
    throw (InternalError ("initializeSymbolicObject: non-symbolic reference '" ++ toString expression ++ "'"))

initializeSymbolicField :: ExecutionState -> DeclarationMember -> Engine r (Identifier, Expression)
initializeSymbolicField state field = do
    let symNameIndex = size (state ^. heap) + 1
    let fieldName@(Identifier oldName pos) = field ^?! SL.name
    let symName = Identifier (oldName ++ show symNameIndex) pos 
    let value   = createSymbolicVar symName (typeOf field)
    return (fieldName, value)
