module Execution.Engine(
    execute
) where

import qualified Data.Set as S
import           Data.Foldable
import           System.Random.Shuffle
import           Polysemy              
import           Polysemy.Reader
import           Polysemy.Cache hiding (Store, Contains)
import           Polysemy.Error hiding (Throw)    
import           Polysemy.State
import           Control.Monad hiding (guard)
import           Control.Monad.Extra
import           Control.Lens hiding (assign)
import           Data.Configuration
import           Data.Error
import           Data.Statistics
import           Language.Syntax
import qualified Language.Syntax.Lenses as SL
import           Text.Pretty
import           Analysis.CFA.CFG
import           Analysis.SymbolTable
import           Execution.Semantics
import           Execution.Semantics.Process
import           Execution.Semantics.Concretization
import           Execution.Semantics.PartialOrderReduction
import           Execution.State
import           Execution.State.Thread
import           Verification.Result

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
            result <- runReader (config, cfg, table) (runError (evalCache (start emptyState initialMethod)))
            case result of
                Left res -> return res
                Right () -> return Valid
        _        -> throw (unknownEntryPointError entryPoint)

start :: ExecutionState -> DeclarationMember -> Engine r ()
start state0 initialMethod = do
    (config, cfg, _) <- ask
    let state1 = state0 & remainingK .~ maximumDepth config
    (state2, tid) <- spawn state1 undefined (context cfg (initialMethod ^?! SL.labels ^. _1) ^. _2) initialMethod arguments
    debug ("Spawning initial thread with thread id '" ++ toString tid ++ "'")
    let state3 = state2 & (currentThreadId ?~ tid)
    -- Add the pre-condition as assumption, branch if it contains an array.
    case initialMethod ^?! SL.specification ^. SL.requires of
        Nothing         -> execP state3
        Just assumption -> do
            states <- execAssume state3 assumption
            branch_ execP states
    where
        -- TODO: add support for non-static and constructors
        arguments            = map createArgument (initialMethod ^?! SL.params)
        createArgument param = createSymbolicVar (param ^?! SL.name) (param ^?! SL.ty)

--------------------------------------------------------------------------------
-- Process Execution 

-- | Symbolically executes the program.
execP :: ExecutionState -> Engine r ()
execP state0 = do
    let allThreads = state0 ^. threads
    debug ("Branching on threads '" ++ toString allThreads ++ "'")
    if null allThreads 
        then finish
        else do
            --applyPOR                <- applyPOR <$> askConfig
            enabledThreads <- filterM (isEnabled state0) (S.toList allThreads)
            when (null enabledThreads) 
                (throw $ Deadlock (state0 ^. programTrace))
            (state1, threads) <- por state0 enabledThreads
            randomExploration <- applyRandomInterleaving <$> askConfig
            if randomExploration
                then branch_ (\ thread -> execT (state1 & (currentThreadId ?~ (thread ^. tid))) (thread ^. pc)) threads
                else do
                    shuffledThreads <- embed (shuffleM enabledThreads)
                    branch_ (\ thread -> execT (state1 & (currentThreadId ?~ (thread ^. tid))) (thread ^. pc)) shuffledThreads

            {-if applyPOR
                then do
                    uniqueThreads  <- filterM isUniqueInterleaving enabledThreads
                    newConstraints <- por enabledThreads
                    updateInterleavingConstraints newConstraints
                    if applyRandomInterleaving
                        then do
                            shuffledThreads <- embed (shuffleM uniqueThreads)
                            branch_ (execT state) shuffledThreads 
                        else branch_ (execT state) uniqueThreads 
                else if applyRandomInterleaving
                    then do
                        shuffledThreads <- embed (shuffleM enabledThreads)
                        branch_ (execT state) shuffledThreads
                    else branch_ (execT state) enabledThreads
            -}
-- branch_ (\ thread -> execT (state & (currentThreadId ?~ (thread ^. tid))) (thread ^. pc)) allThreads

-- TODO: in the above function, for each branch_ call set the ThreadId
{-
isUniqueInterleaving :: Thread -> Engine r Bool
isUniqueInterleaving thread = do
    currentProgramTrace <- getProgramTrace
    not . any (isUnique currentProgramTrace) <$> getInterleavingConstraints
    where
        isUnique currentProgramTrace (IndependentConstraint previous current)
            = (thread ^. pc) == current && previous `elem` currentProgramTrace
        isUnique _ NotIndependentConstraint{}
            = False

updateInterleavingConstraints :: InterleavingConstraints -> Engine r ()
updateInterleavingConstraints newConstraints = do
    oldConstraints <- Prelude.filter (isConflict newConstraints) <$> getInterleavingConstraints
    modifyLocal (\ state -> state & (interleavingConstraints .~ (oldConstraints ++ newConstraints)))

isConflict :: [InterleavingConstraint] -> InterleavingConstraint -> Bool
isConflict _              (IndependentConstraint _ _)      = False
isConflict newConstraints (NotIndependentConstraint x1 y1) =
    any (\case (IndependentConstraint x2 y2)  -> S.fromList [x1, y1] `S.disjoint` S.fromList [x2, y2]
               (NotIndependentConstraint _ _) -> False) newConstraints
    -}
--------------------------------------------------------------------------------
-- Thread Execution

-- | Symbolically executes the thread.
execT :: ExecutionState -> CFGContext -> Engine r ()
execT state0 (_, _, ExceptionalNode, _) = do
    states <- execException state0
    branch_ (uncurry stepM) states

-- A Method Call
execT state0 (_, _, CallNode entry method@Method{} thisInfo arguments lhs, [(_, neighbour)]) = do
    state1 <- execCall state0 method arguments lhs neighbour thisInfo
    step state1 ((), entry)

-- A Constructor Call
execT state0 (_, _, CallNode entry constructor@Constructor{} Nothing arguments lhs, [(_, neighbour)]) = do
    state1 <- execNewObject state0 constructor arguments lhs neighbour
    step state1 ((), entry)

-- A Method or Constructor Call with not exactly one neighbour 
execT _ (_, _, CallNode{}, ns) =
    throw (InternalError ("execT: there should be exactly 1 neighbour, there are '" ++ show (length ns) ++ "'"))

-- A Fork
execT state0 (_, _, ForkNode entry method arguments, neighbours) = do
    state1 <- execFork state0 entry method arguments
    branch_ (step state1) neighbours

-- A Member Entry
execT state0 (_, _, MemberEntry{}, neighbours) = do
    states <- execMemberEntry state0
    -- Continue the execution.
    branch_ ( \ state1 -> branch_ (step state1) neighbours) states

-- A Member Exit
execT state0 (_, _, MemberExit returnTy _ _ _ ensures, []) = do
    states <- execMemberExit state0 returnTy ensures
    branch_ (uncurry stepM) states

execT _ (_, _, MemberExit{}, ns) =
    throw (InternalError ("execT: there should be exactly 0 neighbour, there are '" ++ show (length ns) ++ "'"))

-- A Try Entry
execT state0 (_, _, TryEntry handler, neighbours) = do
    state1 <- execTryEntry state0 handler
    branch_ (step state1) neighbours 

-- A Try Exit
execT state0 (_, _, TryExit, neighbours) = do
    state1 <- execTryExit state0
    branch_ (step state1) neighbours

-- A Catch Entry
execT state0 (_, _, CatchEntry, neighbours) = do
    state1 <- execCatchEntry state0
    branch_ (step state1) neighbours

-- A Catch Exit
execT state (_, _, CatchExit, neighbours) =
    branch_ (step state) neighbours

--------------------------------------------------------------------------------
-- Statement Execution

-- A Declare Statement
execT state0 (_, _, StatNode (Declare ty var _ _), neighbours) = do
    state1 <- execDeclare state0 ty var
    branch_ (step state1) neighbours

execT state0 (_, _, StatNode (Assign lhs rhs _ _), neighbours) = do
    states <- execAssign state0 lhs rhs
    branch_ (\ state1 -> branch_ (step state1) neighbours) states

-- An Assert statement
execT state0 (_, _, StatNode (Assert assertion _ _), neighbours) = do
    states <- execAssert state0 assertion
    branch_ (\ state1 -> branch_ (step state1) neighbours) states

-- An Assume statement
execT state0 (_, _, StatNode (Assume assumption _ _), neighbours) = do
    states <- execAssume state0 assumption
    branch_ (\ state1 -> branch_ (step state1) neighbours) states

-- A Return Statement
execT state0 (_, _, StatNode (Return expression _ _), neighbours) = do
    states <- execReturn state0 expression
    branch_ (\ state1 -> branch_ (step state1) neighbours) states

-- A Lock Statement
execT state0 (_, _, StatNode (Lock var _ _), neighbours) = do
    states <- execLock state0 var
    branch_ ( \ state1 -> branch_ (step state1) neighbours) states

-- An unlock Statement
execT state0 (_, _, StatNode (Unlock var _ _), neighbours) = do
    state1 <- execUnlock state0 var
    branch_ (step state1) neighbours

-- Any other Statement
execT state (_, _, StatNode _, neighbours) =
    branch_ (step state) neighbours

step :: ExecutionState -> ((), Node) -> Engine r ()
step state = stepM state . Just

stepM :: ExecutionState -> Maybe ((), Node) -> Engine r ()
stepM state0 neighbour =
    --nForks <- getNumberOfForks
    --measureMaximumForks nForks
    when (state0 ^. remainingK > 1) $ do
        state1 <- updatePC state0 neighbour
        let state2 = state1 & (remainingK      -~ 1)
                            & (currentThreadId .~ Nothing)
        execP state2

updatePC :: ExecutionState -> Maybe ((), Node) -> Engine r ExecutionState
updatePC state Nothing = 
    return state
updatePC state (Just (_, node)) = do
    cfg <- askCFG
    case getCurrentThread state of
        Nothing      -> 
            return state
        Just thread0 -> do
            let thread1 = thread0 & (pc .~ context cfg node)
            return $ updateThreadInState state thread1 & (programTrace %~ ((thread0 ^. pc) :))

branch :: Foldable t => (a -> Engine r b) -> t a -> Engine r [b]
branch f options = do
    measureBranches options
    foldM' (\ acc value -> do
        result <- catch ((:[]) <$> f value) (\ e -> [] <$ haltInfeasible e)
        return $ result ++ acc) [] (toList options)

branch_ :: Foldable t => (a -> Engine r ()) -> t a -> Engine r ()
branch_ f = void . branch f

--------------------------------------------------------------------------------
-- Branching Functions
--------------------------------------------------------------------------------
{-
branchCS_ :: Thread -> [[Concretization]] -> Engine r () -> Engine r ()
branchCS_ _      []       continueF = continueF
branchCS_ thread (cs:css) continueF = branchC_ thread cs (branchCS_ thread css continueF)

branchC_ :: Thread -> [Concretization] -> Engine r () -> Engine r ()
branchC_ _ [] continueF = continueF
branchC_ _ cs continueF = branch_ (\ concretization -> do
    mapM_ (\ (symRef, concRef) -> setAliases symRef (S.singleton concRef)) (M.toList concretization)
    continueF) cs
-}