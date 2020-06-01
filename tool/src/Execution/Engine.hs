module Execution.Engine(
    execute
) where

import           Prelude hiding (read)
import qualified Data.Set                   as S
import qualified Data.Map                   as M
import qualified Data.HashSet               as H
import           System.Random.Shuffle
import           Data.Maybe
import           Data.Graph.Inductive.Graph         (Node, context)
import           Polysemy              
import           Polysemy.Reader
import           Polysemy.Cache hiding (Store, Contains)
import           Polysemy.Error hiding (Throw)
import           Polysemy.State
import           Control.Monad
import           Control.Monad.Extra
import           Control.Lens
import           Data.Configuration
import           Data.Error
import           Data.Statistics
import           Data.Positioned
import           Syntax.Syntax
import           Syntax.Fold
import           Syntax.DSL
import qualified Syntax.Lenses               as SL
import           Text.Pretty
import           Analysis.CFA.CFG
import           Analysis.SymbolTable
import           Analysis.Type.Typeable
import           Execution.Concurrency.Thread
import           Execution.Concurrency.POR
import           Execution.Concurrency.Lock
import           Execution.ExecutionState
import           Execution.Memory.Heap
import           Execution.Memory.AliasMap
import           Execution.Evaluation
import           Verification.Verifier
import           Verification.Result

import Debug.Trace

--------------------------------------------------------------------------------
-- Symbolic Execution
--------------------------------------------------------------------------------

execute :: Members [Reader Configuration, Error ErrorMessage, State Statistics, Embed IO] r
    => SymbolTable -> ControlFlowGraph -> Sem r VerificationResult
execute table cfg = do
    config@Configuration{entryPoint} <- ask
    symbols <- maybe (throw (unknownEntryPointError entryPoint)) return (lookupConfigurationString entryPoint table)
    case S.toList symbols of
        [symbol] -> do
            let initialMethod = getMember symbol
            result <- runReader (config, cfg, table) (runError (evalCache (evalLocalState emptyState (start initialMethod))))
            case result of
                Left res -> return res
                Right () -> return Valid
        _        -> throw (unknownEntryPointError entryPoint)

start :: DeclarationMember -> Engine r ()
start initialMethod = do
    (config, cfg, _) <- ask
    modifyLocal (\ state -> state & remainingK .~ maximumDepth config)
    thread <- spawnInitialThread (context cfg (initialMethod ^?! SL.labels ^. _1) ^. _2) initialMethod arguments
    -- Add the pre-condition as assumption, branch if it contains an array.
    case initialMethod ^?! SL.specification ^. SL.requires of
        Nothing         -> execP
        Just assumption -> do
            concretizations <- concretesOfType thread ARRAYRuntimeType assumption
            branchC_ thread concretizations $ do
                assume thread assumption
                execP
    where
        -- TODO: add support for non-static and constructors
        arguments            = map createArgument (initialMethod ^?! SL.params)
        createArgument param = createSymbolicVar (param ^?! SL.name) (param ^?! SL.ty)

--------------------------------------------------------------------------------
-- Process Execution 

-- | Symbolically executes the program.
execP :: Engine r ()
execP = do
    allThreads <- getThreads
    if null allThreads 
        then finish
        else do
            applyPOR                <- applyPOR <$> askConfig
            applyRandomInterleaving <- applyRandomInterleaving <$> askConfig
            enabledThreads          <- filterM isEnabled (S.toList allThreads)
            when (null enabledThreads) (do
                programTrace <- getProgramTrace
                throw (Deadlock programTrace))
            if applyPOR
                then do
                    uniqueThreads  <- filterM isUniqueInterleaving enabledThreads
                    newConstraints <- por enabledThreads
                    updateInterleavingConstraints newConstraints
                    if applyRandomInterleaving
                        then do
                            shuffledThreads <- embed (shuffleM uniqueThreads)
                            branch_ execT shuffledThreads 
                        else branch_ execT uniqueThreads 
                else if applyRandomInterleaving
                    then do
                        shuffledThreads <- embed (shuffleM enabledThreads)
                        branch_ execT shuffledThreads
                    else branch_ execT enabledThreads

isUniqueInterleaving :: Thread -> Engine r Bool
isUniqueInterleaving thread = do
    programTrace <- getProgramTrace
    constraints <- getInterleavingConstraints
    return . not . any (isUnique programTrace) $ constraints
    where 
        isUnique programTrace (IndependentConstraint previous current)
            = (thread ^. pc) == current && previous `elem` programTrace
        isUnique _ NotIndependentConstraint{}
            = False

updateInterleavingConstraints :: InterleavingConstraints -> Engine r ()
updateInterleavingConstraints newConstraints = do
    oldConstraints <- filter (isConflict newConstraints) <$> getInterleavingConstraints
    modifyLocal (\ state -> state & (interleavingConstraints .~ (oldConstraints ++ newConstraints)))

isConflict :: [InterleavingConstraint] -> InterleavingConstraint -> Bool
isConflict _              (IndependentConstraint _ _)      = False
isConflict newConstraints (NotIndependentConstraint x1 y1) =
    any (\case (IndependentConstraint x2 y2)  -> x1 == y2 || x2 == y2
               (NotIndependentConstraint _ _) -> False) newConstraints

--------------------------------------------------------------------------------
-- Thread Execution

-- | Symbolically executes the thread.
execT :: Thread -> Engine r ()
-- An Exception
execT thread@Thread{_pc=(_, _, ExceptionalNode, _)}
    -- Within a try block.
    | Just (handler, pops) <- findLastHandler thread
        = case pops of
            -- With no more stack frames to pop.
            0 -> step thread ((), handler)
            -- With some stack frames left to pop
            _ -> do
                -- Verify the exceptional post condition.
                whenM (verifyExceptional <$> askConfig) (do
                    let exceptional = getLastStackFrame thread ^. currentMember ^?! SL.specification ^?! SL.exceptional
                    assertM thread exceptional)
                -- Pop the stack frame and continue executing this thread.
                execT =<< popStackFrame thread
    -- Not within a try block.
    | otherwise = do
        -- Verify the exceptional post condition.
        whenM (verifyExceptional <$> askConfig) (do
            let exceptional = getLastStackFrame thread ^. currentMember ^?! SL.specification ^?! SL.exceptional
            assertM thread exceptional)
        -- Verify all other stack frames in the call stack.
        if isLastStackFrame thread
            then return ()
            else execT =<< popStackFrame thread

-- A Method Call
execT thread0@Thread{_pc=(_, _, CallNode entry method@Method{} variable arguments lhs, [(_, neighbour)])} = do
    -- Construct the parameters and arguments.
    let parameters = method ^?! SL.params
    arguments' <- maybe (return []) (fmap (:[]) . readVar thread0) variable >>= (return . (++arguments))
    -- Push a new stack frame and continue the execution.
    thread1 <- pushStackFrame thread0 thread0 neighbour method lhs (zip parameters arguments')
    step thread1 ((), entry)

-- A Constructor Call
execT thread0@Thread{_pc=(_, _, CallNode entry constructor@Constructor{} Nothing arguments lhs, [(_, neighbour)])} = do
    -- Construct the parameters, with 'this' as an implicit parameter.
    let className  = constructor ^?! SL.name
    let parameters = parameter' (refType' className) this' : constructor ^?! SL.params
    -- Allocate a new reference initialized with concrete default values.
    fields <- map getMember . S.toList . getAllFields className <$> askTable
    let structure = ObjectValue ((M.fromList . map (\ Field{..} -> (_name, defaultValue _ty))) fields) (typeOf constructor)
    ref <- allocate structure
    let arguments' = ref : arguments
    -- Concretize the necessary arguments.
    concretizations <- mapM (concretesOfType thread0 ARRAYRuntimeType) arguments'
    branchCS_ thread0 concretizations $ do
        -- Push a new stack frame and continue the execution.
        thread1 <- pushStackFrame thread0 thread0 neighbour constructor lhs (zip parameters arguments')
        step thread1 ((), entry)
    
-- A Fork
execT thread@Thread{_pc=(_, _, ForkNode entry method arguments, [neighbour])} = do
    -- Spawn a new thread and continue the execution.
    _ <- spawn thread entry method arguments
    step thread neighbour

-- A Member Entry
execT thread@Thread{_pc=(_, _, MemberEntry{}, neighbours)} = do
    -- Verify the pre condition if this is not the first call.
    programTrace <- getProgramTrace
    case programTrace of
        [] -> return ()
        _  -> whenM (verifyRequires <$> askConfig) (do
                let requires = getLastStackFrame thread ^. currentMember ^?! SL.specification ^?! SL.requires
                assertM thread requires)
    -- Continue the execution.
    branch_ (step thread) neighbours

-- A Member Exit
execT thread0@Thread{_pc=(adj, node, MemberExit returnTy a b c ensures, [])}
    -- The last stack frame, this thread is finished.
    | isLastStackFrame thread0 = do
        generateEnsureVC <- verifyEnsures <$> askConfig
        if generateEnsureVC && isJust ensures 
            then do
                concretizations <- concretesOfType thread0 ARRAYRuntimeType (fromJust ensures)
                branchC_ thread0 concretizations $ do
                    assertM thread0 ensures
                    despawn thread0
                    stepM thread0 Nothing
            else do
                despawn thread0
                stepM thread0 Nothing
    -- Not the last stack frame, continue executing the thread.
    | otherwise = do
        generateEnsureVC <- verifyEnsures <$> askConfig
        if generateEnsureVC && isJust ensures
            then do
                concretizations <- concretesOfType thread0 ARRAYRuntimeType (fromJust ensures)
                branchC_ thread0 concretizations $ do
                    assertM thread0 ensures
                    execT (thread0{_pc=(adj, node, MemberExit returnTy a b c Nothing, [])})
            else do
                let oldFrame  = getLastStackFrame thread0
                let neighbour = ((), oldFrame ^. returnPoint)
                -- Remove the top most stack frame from the stack.
                thread1 <- popStackFrame thread0
                -- Check if we need te assign the return value to some lhs
                case oldFrame ^. lhs of
                    Just lhs -> do
                        -- Copy the retval to the new stack frame and continue with an assignment.
                        let assign = assign' lhs (rhsExpr' (var' retval' returnTy))
                        value <- readVar thread0 retval'
                        thread2 <- writeAndSubstituteVar thread1 retval' value
                        execT thread2{_pc=(adj, node, StatNode assign, [neighbour])}
                    Nothing  -> 
                        -- No assignment to be done, continue the execution.
                        step thread1 neighbour
 
-- A Try Entry
execT thread0@Thread{_pc=(_, _, TryEntry handler, neighbours)} = do
    thread1 <- insertHandler thread0 handler
    branch_ (step thread1) neighbours 

-- A Try Exit
execT thread0@Thread{_pc=(_, _, TryExit, neighbours)} = do
    thread1 <- removeLastHandler thread0
    branch_ (step thread1) neighbours

-- A Catch Entry
execT thread0@Thread{_pc=(_, _, CatchEntry, neighbours)} = do
    thread1 <- removeLastHandler thread0
    branch_ (step thread1) neighbours

-- A Catch Exit
execT thread@Thread{_pc=(_, _, CatchExit, neighbours)} =
    branch_ (step thread) neighbours

--------------------------------------------------------------------------------
-- Statement Execution

-- A Declare Statement
execT thread0@Thread{_pc=(_, _, StatNode (Declare ty var _ _), neighbours)} = do
    thread1 <- writeVar thread0 var (defaultValue ty)
    branch_ (step thread1) neighbours

-- An Assign Statement
execT thread0@Thread{_pc=(_, _, StatNode (Assign lhs rhs _ _), neighbours)} =
    case rhs of
        RhsCall{} 
            -> branch_ (step thread0) neighbours
        _   -> execRhs thread0 rhs $ \ value ->
                case lhs of
                    LhsVar{} -> do
                        thread1 <- writeAndSubstituteVar thread0 (lhs ^?! SL.var) value
                        branch_ (step thread1) neighbours
                    LhsField{} -> do
                        let field = lhs ^?! SL.field
                        ref <- readVar thread0 (lhs ^?! SL.var)
                        processRef ref
                            (\ _ -> do
                                writeField (ref ^?! SL.ref) field value
                                branch_ (step thread0) neighbours)
                            (\ _ -> do
                                aliases <- fromJust <$> getAliasesWithoutNull (ref ^?! SL.var)
                                mapM_ (\ concRef -> writeSymbolicField ref concRef field value) aliases
                                branch_ (step thread0) neighbours)
                            infeasible
                    LhsElem{} -> do
                        ref <- readVar thread0 (lhs ^?! SL.var)
                        processRef ref
                            (\ concRef -> do
                                let index0 = lhs ^?! SL.index
                                evaluatedIndex <- evaluateAsInt thread0 index0
                                case evaluatedIndex of
                                    Right evaluatedIndex ->
                                        writeIndex concRef evaluatedIndex value
                                    Left index1 -> 
                                        writeIndexSymbolic concRef index1 value
                                branch_ (step thread0) neighbours)
                            (\ symRef -> do
                                concretizations <- concretesOfType thread0 ARRAYRuntimeType symRef
                                branchC_ thread0 concretizations (execT thread0))
                            infeasible

-- An Assert statement
execT thread@Thread{_pc=(_, _, StatNode (Assert assertion _ _), neighbours)} = do
    concretizations <- concretesOfType thread ARRAYRuntimeType assertion
    branchC_ thread concretizations $ do
        assert thread assertion
        branch_ (step thread) neighbours

-- An Assume statement
execT thread@Thread{_pc=(_, _, StatNode (Assume assumption _ _), neighbours)} = do
    concretizations <- concretesOfType thread ARRAYRuntimeType assumption
    branchC_ thread concretizations $ do
        assume thread assumption
        branch_ (step thread) neighbours

-- A Return Statement
execT thread0@Thread{_pc=(_, _, StatNode (Return expression _ _), neighbours)} = do
    concretizations <- concretesOfTypeM thread0 ARRAYRuntimeType expression
    branchC_ thread0 concretizations $ do
        thread1 <- maybe (return thread0) (writeAndSubstituteVar thread0 retval') expression
        branch_ (step thread1) neighbours

-- A Lock Statement
execT thread@Thread{_pc=(_, _, StatNode (Lock var _ _), neighbours)} = do
    ref <- readVar thread var
    processRef ref
        (\ concRef -> do
            isLocked <- locked (thread ^. tid) concRef
            if isLocked
                -- The reference is locked, stop the exploration of this branch.
                then infeasible
                -- The reference is not locked, add to the lock set and continue execution.
                else do
                    lock (thread ^. tid) concRef
                    branch_ (step thread) neighbours)
        (\ symRef -> do
            concretizations <- concretesOfType thread ARRAYRuntimeType symRef
            branchC_ thread concretizations (execT thread))
        infeasible

-- An Unlock Statement
execT thread@Thread{_pc=(_, _, StatNode (Unlock var _ _), neighbours)} = do
    value <- readVar thread var
    case value of
        Ref ref _ _ -> do
            unlock ref
            branch_ (step thread) neighbours
            
-- Any other Statement
execT thread@Thread{_pc=(_, _, StatNode _, neighbours)} =
    branch_ (step thread) neighbours

step :: Thread -> ((), Node) -> Engine r ()
step thread = stepM thread . Just

stepM :: Thread -> Maybe ((), Node) -> Engine r ()
stepM thread0 neighbour = do
    nForks <- getNumberOfForks
    measureMaximumForks nForks
    k <- getRemainingK
    when (k > 1) $ do
        case neighbour of
            Just (_, node) -> do
                cfg <- askCFG
                let thread1 = thread0 & (pc .~ context cfg node)
                modifyLocal (\ state -> state & (threads %~ S.insert thread1))
            _ -> return ()
        modifyLocal (\ state -> state & (remainingK   -~ 1)
                                      & (programTrace %~ ((thread0 ^. pc) :)))
        execP
    
execRhs :: Thread -> Rhs -> (Expression -> Engine r ()) -> Engine r ()
execRhs thread rhs@RhsExpression{} continueF = do
    concretizations <- concretesOfType thread ARRAYRuntimeType (rhs ^?! SL.value)
    branchC_ thread concretizations $ do
        localSolving <- applyLocalSolver <$> askConfig
        if localSolving
            then do
                value' <- evaluate thread (rhs ^?! SL.value)
                debug ("Evaluated rhs '" ++ toString rhs ++ "' to '" ++ toString value' ++ "'")
                continueF value'
            else substitute thread (rhs ^?! SL.value) >>= continueF

execRhs thread rhs@RhsField{} continueF = do
    ref <- readVar thread (rhs ^?! SL.var ^?! SL.var)
    let field = rhs ^?! SL.field
    case ref of
        Ref{}             -> readField (ref ^?! SL.ref) field >>= continueF
        SymbolicRef{}     -> initializeSymbolicRef ref >> readSymbolicField ref field >>= continueF
        Lit NullLit{} _ _ -> infeasible
        IteE{}            -> processRhsFieldIte field ref >>= continueF

execRhs thread rhs@RhsElem{} continueF = do
    ref <- readVar thread (rhs ^?! SL.var ^?! SL.var)
    processRef ref
        (\ concRef -> do
            evaluatedIndex <- evaluateAsInt thread (rhs ^?! SL.index)
            value <- either (readIndexSymbolic concRef) (readIndex concRef) evaluatedIndex
            continueF value)
        (error "execRhs: Symbolic Reference")
        infeasible

execRhs thread rhs@RhsArray{} continueF = do
    sizes <- mapM (evaluateAsInt thread) (rhs ^?! SL.sizes)
    value <- createArray sizes (typeOf rhs)
    continueF value

processRhsFieldIte :: Identifier -> Expression -> Engine r Expression
processRhsFieldIte field (IteE guard true false ty info) = do
    true'  <- processRhsFieldIte field true
    false' <- processRhsFieldIte field false
    return $ IteE guard true' false' ty info
processRhsFieldIte _ (Lit NullLit{} _ _) 
    = infeasible
processRhsFieldIte field (Ref ref _ _) 
    = readField ref field
processRhsFieldIte field ref@SymbolicRef{}
    = readSymbolicField ref field

--------------------------------------------------------------------------------
-- Verification Functions
--------------------------------------------------------------------------------

-- | Assume that the given expression holds for the rest of the execution.
assume :: Thread -> Expression -> Engine r ()
assume thread assumption0 = do
    localSolving <- applyLocalSolver <$> askConfig
    if localSolving
        then do
            assumption1 <- evaluateAsBool thread assumption0
            case assumption1 of
                Right True  -> return ()
                Right False -> do
                    assumptionDebug <- substitute thread assumption0
                    debug ("Constraint '" ++ toString assumptionDebug ++ "' is infeasible")
                    infeasible
                Left assumption2 -> do
                    debug ("Adding constraint: '" ++ toString assumption2 ++ "'")
                    modifyLocal (\ state -> state & (constraints %~ H.insert assumption2))
        else do
            assumption1 <- substitute thread assumption0
            debug ("Adding constraint: '" ++ toString assumption1 ++ "'")
            modifyLocal (\ state -> state & (constraints %~ H.insert assumption1))

assertM :: Thread -> Maybe Expression -> Engine r ()
assertM thread = maybe (return ()) (assert thread)

-- | Verify that the given that the given expression holds.
assert :: Thread -> Expression -> Engine r ()
assert thread assertion = do
    measureVerification
    assumptions  <- getConstraints
    config       <- askConfig
    programTrace <- getProgramTrace
    let formula0 = ands' (neg' assertion `H.insert` assumptions)
    debug ("Verifying: '" ++ toString formula0 ++ "'")
    if applyLocalSolver config
        then do
            formula1 <- evaluateAsBool thread formula0
            case formula1 of
                Right True -> do
                    measureLocalSolve
                    programTrace <- getProgramTrace
                    throw (Invalid (getPos assertion) programTrace)
                Right False -> do
                    measureLocalSolve
                    return ()
                Left formula2 -> do
                    aliasMap <- getAliasMap
                    let verification = verify programTrace aliasMap (getPos assertion) formula2
                    if cacheFormulas config 
                        then underCache formula2 verification
                        else verification
        else do
            formula1 <- substitute thread formula0
            aliasMap <- getAliasMap
            let verification = verify programTrace aliasMap (getPos assertion) formula1
            if cacheFormulas config
                then underCache formula1 verification
                else verification

--------------------------------------------------------------------------------
-- Concretization- and Branching Functions
--------------------------------------------------------------------------------

branchCS_ :: Thread -> [[Concretization]] -> Engine r () -> Engine r ()
branchCS_ _      []       continueF = continueF
branchCS_ thread (cs:css) continueF = branchC_ thread cs (branchCS_ thread css continueF)

branchC_ :: Thread -> [Concretization] -> Engine r () -> Engine r ()
branchC_ _      [] continueF = continueF
branchC_ thread cs continueF = branch_ (\ concretization -> do
    mapM_ (\ (symRef, concRef) -> setAliases symRef (S.singleton concRef)) (M.toList concretization)
    continueF) cs

concretesOfTypeM :: Thread -> RuntimeType -> Maybe Expression -> Engine r [Concretization]
concretesOfTypeM thread ty = maybe (return []) (concretesOfType thread ty)

concretesOfType :: Thread -> RuntimeType -> Expression -> Engine r [Concretization]
concretesOfType thread ty formula = do
    refs <- findSymbolicRefsOfType thread ty formula
    mapM_ initializeSymbolicRef refs
    if not (null refs)
        then do
            aliasMap <- getAliasMap
            let mappings = map (\ ref -> map (ref ^?! SL.var, ) (S.toList (aliasMap M.! (ref ^?! SL.var)))) (S.toList refs)
            return $ map M.fromList (cartesianProduct mappings)
        else return []

findSymbolicRefsOfType :: Thread -> RuntimeType -> Expression -> Engine r (S.Set Expression)
findSymbolicRefsOfType thread ty = foldExpression algebra
    where
        algebra = monoidMExpressionAlgebra
            { fForall = \ _ _ domain formula _ _ -> do
                ref <- readVar thread domain
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

            , fSizeOf = \ var _ _ -> do
                ref <- readVar thread var
                processRef ref
                    (const (return S.empty))
                    (const (return (S.singleton ref))) -- TODO: this does not account for type (always correct as only used for array and this is sizeof)
                    (return S.empty)
                        
            , fSymRef = \ symVar varTy varPos -> return $ if varTy `isOfType` ty 
                then S.singleton (SymbolicRef symVar varTy varPos) 
                else S.empty }