module Execution.Semantics(
      execAssert
    , execAssume
    , execReturn
    , execCall
    , execNewObject
    , execFork
    , execMemberEntry
    , execMemberExit
    , execException
    , execTryEntry
    , execTryExit
    , execCatchEntry 
    , execDeclare
    , execLock
    , execUnlock
    , execAssign
) where
       
import qualified Data.Set as S
import qualified Data.Map as M
import           Data.Maybe
import           Control.Monad (foldM)
import           Control.Lens ((&), (^?!), (^.), (%~), (<>~))
import           Control.Monad.Extra
import           Polysemy.Cache
import           Text.Pretty
import           Data.Configuration
import           Data.Statistics
import           Data.Positioned
import           Analysis.Type.Typeable
import           Analysis.CFA.CFG
import           Analysis.SymbolTable
import           Language.Syntax
import           Language.Syntax.DSL
import qualified Language.Syntax.Lenses as SL
import           Execution.Semantics.Evaluation
import           Execution.Semantics.Thread
import           Execution.Semantics.Exception
import           Execution.Semantics.Concretization
import           Execution.Semantics.StackFrame
import           Execution.Semantics.Heap
import           Execution.Semantics.Process
import           Execution.State
import           Execution.State.Thread
import           Execution.State.Heap
import           Execution.State.PathConstraints as PathConstraints
import           Execution.State.LockSet as LockSet
import           Execution.State.AliasMap as AliasMap
import           Verification.Verifier

execAssert :: ExecutionState -> Expression -> Engine r [ExecutionState]
execAssert state0 assertion = do
    measureVerification
    let assumptions = state0 ^. constraints
    config              <- askConfig
    let currentProgramTrace = state0 ^. programTrace
    (state1, concretizations) <- concretesOfType state0 ARRAYRuntimeType assertion
    concretize concretizations state1 $ \ state2 -> do
        let formula0 = asExpression (PathConstraints.insert (neg' assertion) assumptions)
        debug ("Verifying: '" ++ toString formula0 ++ "'")
        (state3, formula1) <- evaluateAsBool state2 formula0
        case formula1 of
            Right True ->
                invalid state3 assertion
            Right False ->
                return state3
            Left formula2 -> do
                let verification = verify currentProgramTrace (state3 ^. aliasMap) (getPos assertion) formula2
                if cacheFormulas config 
                    then underCache formula2 verification
                    else verification
                return state3

execAssertEnsures :: ExecutionState -> Engine r [ExecutionState]
execAssertEnsures state = do
    config <- askConfig
    if verifyRequires config
        then
            case getCurrentThread state of
                Just thread -> do
                    let frame   = fromJust (getLastStackFrame thread)
                    let ensures = frame ^. currentMember ^?! SL.specification ^?! SL.ensures
                    maybe (return [state]) (execAssert state) ensures
                Nothing     ->
                    stop state "execAssertEnsures: cannot get current thread"
        else 
            return [state]

execAssertRequires :: ExecutionState -> Engine r [ExecutionState]
execAssertRequires state = do
    config <- askConfig
    if verifyRequires config
        then
            case getCurrentThread state of
                Just thread -> do
                    let frame    = fromJust (getLastStackFrame thread)
                    let requires = frame ^. currentMember ^?! SL.specification ^?! SL.requires
                    maybe (return [state]) (execAssert state) requires
                Nothing     ->
                    stop state "execAssertRequires: cannot get current thread"
        else 
            return [state]

execAssertExceptional :: ExecutionState -> Engine r [ExecutionState]
execAssertExceptional state = do
    config <- askConfig
    if verifyExceptional config
        then
            case getCurrentThread state of
                Just thread -> do
                    let frame       = fromJust (getLastStackFrame thread)
                    let exceptional = frame ^. currentMember ^?! SL.specification ^?! SL.exceptional
                    maybe (return [state]) (execAssert state) exceptional
                Nothing     ->
                    stop state "execAssertExceptional: cannot get current thread"
        else
            return [state]

execAssume :: ExecutionState -> Expression -> Engine r [ExecutionState]
execAssume state0 assumption0 = do
    (state1, concretizations) <- concretesOfType state0 ARRAYRuntimeType assumption0
    concretize concretizations state1 $ \ state2 -> do
        (state3, assumption1) <- evaluateAsBool state2 assumption0
        case assumption1 of
            Right True  ->  
                return state3
            Right False -> do
                debug "Constraint is infeasible"
                infeasible
            Left assumption2 -> do
                debug ("Adding constraint: '" ++ toString assumption2 ++ "'")
                return $ state3 & (constraints <>~ PathConstraints.singleton assumption2)

execCall :: ExecutionState -> DeclarationMember -> [Expression] -> Maybe Lhs -> Node -> Maybe (NonVoidType, Identifier) -> Engine r ExecutionState
execCall state method arguments lhs neighbour thisInfo = do
    -- Construct the parameters and arguments.
    let parameters = [Parameter (fst (fromJust thisInfo)) this' unknownPos | isJust thisInfo] ++ method ^?! SL.params
    let arguments' = [var' (snd (fromJust thisInfo)) (typeOf (fst (fromJust thisInfo))) | isJust thisInfo] ++ arguments
    -- Push a new stack frame.
    pushStackFrameOnCurrentThread state neighbour method lhs (zip parameters arguments')

execNewObject :: ExecutionState -> DeclarationMember -> [Expression] -> Maybe Lhs -> Node -> Engine r ExecutionState
execNewObject state0 constructor arguments lhs neighbour  = do
    -- Construct the parameters, with 'this' as an implicit parameter.
    let className  = constructor ^?! SL.name
    let parameters = parameter' (refType' className) this' : constructor ^?! SL.params
    -- Allocate a new reference initialized with concrete default values.
    fields <- map getMember . S.toList . getAllFields className <$> askTable
    let structure = ObjectValue ((M.fromList . map (\ Field{..} -> (_name, defaultValue _ty))) fields) (typeOf constructor)
    (state1, ref) <- allocate state0 structure
    let arguments' = ref : arguments
    -- Push a new stack frame.
    pushStackFrameOnCurrentThread state1 neighbour constructor lhs (zip parameters arguments')

execFork :: ExecutionState -> Node -> DeclarationMember -> [Expression] -> Engine r (ExecutionState, ThreadId)
execFork state entry member arguments
    | Just parent <- state ^. currentThreadId =
        spawn state parent entry member arguments
    | otherwise = 
        stop state "execFork: cannot get current thread"

execMemberEntry :: ExecutionState -> Engine r [ExecutionState]
execMemberEntry state =
    -- Verify the pre condition if this is the first call.
    case state ^. programTrace of
        [] -> return [state]
        _  -> execAssertRequires state

execMemberExit :: ExecutionState -> RuntimeType -> Engine r [(ExecutionState, Maybe ((), Node))]
execMemberExit state0 returnTy = do
    -- Verify the post-condition
    states <- execAssertEnsures state0
    concatForM states $ \ state1 ->   
        if isLastStackFrame state1
            -- Despawn if this is the last stack frame to be popped
            then do
                state2 <- despawnCurrentThread state1
                return [(state2, Nothing)]
            -- Otherwise pop the stack frame with a copied return value.
            else
                case getCurrentThread state1 of
                    Nothing ->
                            stop state1 "execMemberExit: cannot get current thread"
                    Just thread1 -> do
                        let oldFrame  = fromJust (getLastStackFrame thread1)
                        let neighbour = Just ((), oldFrame ^. returnPoint)
                        state2 <- popStackFrame state1
                        -- Check if we need te assign the return value to some target
                        case oldFrame ^. target of
                            Just lhs -> do
                                retval <- readDeclaration state1 retval'
                                state3 <- writeDeclaration state2 retval' retval
                                let rhs = rhsExpr' (var' retval' returnTy)
                                map (, neighbour) <$> execAssign state3 lhs rhs
                            Nothing  -> 
                                -- No assignment to be done, continue the execution.
                                return [(state2, neighbour)]

execReturn :: ExecutionState -> Maybe Expression -> Engine r [ExecutionState]
execReturn state Nothing =
    return [state]

execReturn state0 (Just expression) = do
    (state1, concretizations) <- concretesOfType state0 ARRAYRuntimeType expression
    concretize concretizations state1 $ \ state2 -> do
        (state3, retval) <- evaluate state2 expression
        writeDeclaration state3 retval' retval

execException :: ExecutionState -> Engine r [(ExecutionState, Maybe ((), Node))]
execException state0
    -- Within a try block.
    | Just (handler, pops) <- findLastHandler state0 = do
        debug ("Handling an exception with '" ++ show pops ++ "' left") 
        case pops of
            -- With no more stack frames to pop.
            0 -> 
                return [(state0, Just ((), handler))]
            -- With some stack frames left to pop
            _ -> do
                states <- execAssertExceptional state0
                concatForM states $ \ state1 -> do
                    state2 <- popStackFrame state1
                    execException state2
    -- Not within a try block.
    | otherwise = do
        states <- execAssertExceptional state0
        concatForM states $ \ state1 ->
            if isLastStackFrame state1
                then 
                    return []
                else do
                    state2 <- popStackFrame state1
                    execException state2

execTryEntry :: ExecutionState -> Node -> Engine r ExecutionState
execTryEntry = insertHandler

execTryExit :: ExecutionState -> Engine r ExecutionState
execTryExit = removeLastHandler

execCatchEntry :: ExecutionState -> Engine r ExecutionState
execCatchEntry = removeLastHandler

execDeclare :: ExecutionState -> NonVoidType -> Identifier -> Engine r ExecutionState
execDeclare state0 ty var = do
    let value = defaultValue ty
    writeDeclaration state0 var value

execLock :: ExecutionState -> Identifier -> Engine r [ExecutionState]
execLock state0 var = do
    ref <- readDeclaration state0 var 
    case ref of
        Lit NullLit{} _ _ -> infeasible
        SymbolicRef{}     -> do
            (state1, concretizations) <- concretesOfType state0 ARRAYRuntimeType ref
            concretizes concretizations state1 $ \ state2 ->
                execLock state2 var
        Ref ref _ _       ->
            case state0 ^. currentThreadId of
                Nothing         -> 
                    stop state0 "execLock: cannot get current thread"
                Just currentTid -> 
                    case LockSet.lookup ref (state0 ^. locks) of
                        Just tid -> if tid == currentTid then return [state0] else infeasible
                        Nothing  -> return [state0 & (locks %~ LockSet.insert ref currentTid)]
        _                 -> 
            stop state0 "execLock: expected a reference"

execUnlock :: ExecutionState -> Identifier -> Engine r ExecutionState
execUnlock state var = do
    ref <- readDeclaration state var
    case ref of
        Lit NullLit{} _ _ -> stop state "execT: null in unlock statement."
        SymbolicRef{}     -> stop state "execT: symbolic reference in unlock statement."
        Ref ref _ _       -> return $ state & (locks %~ LockSet.remove ref)
        _                 -> stop state "execUnlock: expected a reference"

execAssign :: ExecutionState -> Lhs -> Rhs -> Engine r [ExecutionState]
execAssign state0 _ RhsCall{} = 
    return [state0]
execAssign state0 lhs rhs = do
    states <- execRhs state0 rhs
    concatMapM (\ (state1, value) -> execLhs state1 lhs value) states

execLhs :: ExecutionState -> Lhs -> Expression -> Engine r [ExecutionState]
execLhs state0 lhs@LhsVar{} value = do
    state1 <- writeDeclaration state0 (lhs ^?! SL.var) value
    return [state1]

execLhs state lhs@LhsField{} value = do
    let field = lhs ^?! SL.field
    ref <- readDeclaration state (lhs ^?! SL.var)
    case ref of
        Lit NullLit{} _ _ -> infeasible
        Ref ref _ _       -> (:[]) <$> writeConcreteField state ref field value
        SymbolicRef{}     -> (:[]) <$> writeSymbolicField state ref field value
        _                 -> stop state "execLhs: expected a reference"
            
execLhs state0 lhs@LhsElem{} value = do
    ref <- readDeclaration state0 (lhs ^?! SL.var)
    case ref of
        Lit NullLit{} _ _ -> infeasible
        Ref ref _ _       -> do
            (state1, index) <- evaluateAsInt state0 (lhs ^?! SL.index)
            case index of
                Right index -> (:[]) <$> writeConcreteElem state1 ref index value
                Left index  -> (:[]) <$> writeSymbolicElem state1 ref index value
        SymbolicRef symRef _ _ -> do
            (state1, concretizations) <- concretesOfType state0 ARRAYRuntimeType ref
            concretize concretizations state1 $ \ state2 ->
                case AliasMap.lookup symRef (state2 ^. aliasMap) of
                    Nothing   -> 
                        stop state2 "execLhs: No concrete references"
                    Just refs ->
                        if length refs /= 1
                            then stop state2 "execLhs: Non concretized symbolic reference"
                            else do
                                (state3, index) <- evaluateAsInt state2 (lhs ^?! SL.index)
                                let (Ref ref _ _) = S.elemAt 0 refs
                                case index of
                                    Right index -> writeConcreteElem state3 ref index value
                                    Left index  -> writeSymbolicElem state3 ref index value
        _                      ->
            stop state0 "execLhs: expected a reference"

execRhs :: ExecutionState -> Rhs -> Engine r [(ExecutionState, Expression)]
execRhs state0 rhs@RhsExpression{} = do
    (state1, concretizations) <- concretesOfType state0 ARRAYRuntimeType (rhs ^?! SL.value)
    concretizeWithResult concretizations state1 $ \ state2 -> do
        (state3, value) <- evaluate state2 (rhs ^?! SL.value)
        debug ("Evaluated rhs '" ++ toString rhs ++ "' to '" ++ toString value ++ "'")
        return (state3, value)

execRhs state0 rhs@RhsField{} = do
    let field = rhs ^?! SL.field
    ref <- readDeclaration state0 (rhs ^?! SL.var ^?! SL.var)
    case ref of
        Lit NullLit{} _ _ -> 
            infeasible
        Ref ref _ _       -> do
            value <- readConcreteField state0 ref field
            return [(state0, value)]
        SymbolicRef{}     -> do
            state1 <- initializeSymbolicRef state0 ref
            value  <- readSymbolicField state1 ref field
            return [(state1, value)]
        Conditional{}     -> do
            value <- execRhsFieldConditional ref field
            return [(state0, value)]
        _                 -> 
            stop state0 "execRhs: expected a reference or conditional"
        where
            execRhsFieldConditional :: Expression -> Identifier -> Engine r Expression
            execRhsFieldConditional (Conditional guard true0 false0 ty info) field = do
                true1  <- execRhsFieldConditional true0 field
                false1 <- execRhsFieldConditional false0 field
                return $ Conditional guard true1 false1 ty info
            execRhsFieldConditional (Lit NullLit{} _ _) _ = 
                infeasible
            execRhsFieldConditional (Ref ref _ _) field = 
                readConcreteField state0 ref field
            execRhsFieldConditional ref@SymbolicRef{} field =
                readSymbolicField state0 ref field
            execRhsFieldConditional _ _ =
                stop state0 "execRhsFieldConditional: expected a reference or conditional"
  
execRhs state0 rhs@RhsElem{} = do
    let var = rhs ^?! SL.var ^?! SL.var
    ref <- readDeclaration state0 var
    case ref of
        Lit NullLit{} _ _   -> infeasible
        SymbolicRef ref _ _ -> 
            case AliasMap.lookup ref (state0 ^. aliasMap) of
                Nothing      -> 
                    stop state0 "execRhs: no aliases"
                Just aliases -> 
                    if S.size aliases /= 1
                        then stop state0 "execRhs: Symbolic Reference"
                        else do
                            let alias = S.elemAt 0 aliases
                            debug ("Overwritting '" ++ toString var ++ "' with single alias '" ++ toString alias ++ "'")
                            state1 <- writeDeclaration state0 var alias
                            execRhs state1 rhs
        Ref ref _ _         -> do
            (state1, index) <- evaluateAsInt state0 (rhs ^?! SL.index)
            value <- either (readSymbolicElem state1 ref) (readConcreteElem state1 ref) index
            return [(state1, value)]
        _                   ->
            stop state0 "execRhs: expected a reference"

execRhs state0 rhs@RhsArray{} = do 
    (state1, sizes) <- mapAccumM evaluateAsInt state0 (rhs ^?! SL.sizes)
    (:[]) <$> execNewArray state1 sizes (typeOf rhs)

execRhs state RhsCall{} =
    stop state "execRhs: Evaluating a method call"

execNewArray :: ExecutionState -> [EvaluationResult Int] -> RuntimeType -> Engine r (ExecutionState, Expression)
execNewArray state [Right size] ty = do
    let elemTy    = ty ^?! SL.innerTy
    let structure = ArrayValue (replicate size (defaultValue elemTy))
    allocate state structure

execNewArray state0 (Right size:sizes) ty = do
    (state1, refs) <- foldM (const . execNewInnerArray) (state0, []) [0..size-1]
    let structure = ArrayValue refs
    allocate state1 structure
    where
        execNewInnerArray :: (ExecutionState, [Expression]) -> Engine r (ExecutionState, [Expression])
        execNewInnerArray (state1, refs) = do
            let elemTy = ty ^?! SL.innerTy
            (state2, ref) <- execNewArray state1 sizes elemTy
            return (state2, ref : refs)

execNewArray state _ _ =
    stop state "execNewArray: symbolic size"