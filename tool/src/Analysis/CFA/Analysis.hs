module Analysis.CFA.Analysis where

import           Polysemy        
import           Polysemy.State
import           Polysemy.Reader
import           Polysemy.Error                  (Error, throw)
import           Control.Monad
import qualified Data.Set                   as S
import           Control.Lens                    hiding (from, to)
import           Control.Lens.Extras
import           Data.Graph.Inductive.Graph      (insEdge, insNode)
import           Data.Error
import           Language.Syntax
import           Language.Syntax.DSL
import qualified Language.Syntax.Lenses     as SL
import           Analysis.Type.Typeable
import           Analysis.CFA.CFG
import           Analysis.SymbolTable

--------------------------------------------------------------------------------
-- Control Flow Analysis
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- Compilation Unit, Declaration and Member

constructCompilationUnit :: Members [Error ErrorMessage, Reader SymbolTable, State ControlFlowGraph] r 
    => CompilationUnit -> Sem r ControlFlowGraph
constructCompilationUnit program = do
    table <- ask
    mapM_ insertMemberN (getAllMethods table)
    insertExceptionalN 
    mapM_ constructDeclaration (program ^. SL.members)
    get

constructDeclaration :: Members [Error ErrorMessage, Reader SymbolTable, State ControlFlowGraph] r
    => Declaration -> Sem r ()
constructDeclaration declaration
    = mapM_ (constructMember declaration) (declaration ^. SL.members)

initMember :: Identifier -> DeclarationMember -> CFGNode
initMember className method 
    = (label, MemberEntry (typeOf method) className methodName params)
    where
        label      = method ^?! SL.labels ^. _1
        methodName = method ^?! SL.name
        params     = map (\ param -> typeOf (param ^?! SL.ty)) (method ^?! SL.params)

finalMember :: Identifier -> DeclarationMember -> CFGNodes
finalMember className method
    = S.singleton (label, MemberExit (typeOf method) className methodName params)
    where
        label      = method ^?! SL.labels ^. _2
        methodName = method ^?! SL.name
        params     = map (\ param -> typeOf (param ^?! SL.ty)) (method ^?! SL.params)

constructMember :: Members [Error ErrorMessage, State ControlFlowGraph] r 
    => Declaration -> DeclarationMember -> Sem r ()
constructMember _ Field{} = return ()
constructMember declaration method = do
    constructStatement method body
    insertE  (initMember className method) (initStatement body)
    insertEs (finalStatement body) (S.findMin $ finalMember className method)
    unless (null others)
        (let (StatNode stat) = S.findMin others ^. _2
          in throw (continueOrBreakNotInWhileError stat))
    insertEs returns (S.findMin $ finalMember className method)
    where
        body                 = method ^?! SL.body
        (returns, others)    = S.partition isReturnN (fallthrough body)
        className            = declaration ^. SL.name

initStatement :: Statement -> CFGNode
initStatement s@Seq{}   = initStatement (s ^?! SL.stat1)
initStatement s@Block{} = initStatement (s ^?! SL.body)
initStatement s@Try{}   = toTryEntryNode s
initStatement s         = toNode s

finalStatement :: Statement -> CFGNodes
finalStatement s@Assign{} = if SL._RhsCall `is` (s ^?! SL.rhs)
                                then finalInvocationCall (Just (s ^?! SL.lhs)) (s ^?! SL.rhs ^?! SL.invocation)
                                else S.singleton (toNode s)
finalStatement s@Call{}    = finalInvocationCall Nothing (s ^?! SL.invocation)
finalStatement s@Ite{}     = finalStatement (s ^?! SL.trueBody) `S.union` finalStatement (s ^?! SL.falseBody)
finalStatement Continue{}  = S.empty
finalStatement Break{}     = S.empty
finalStatement Return{}    = S.empty
finalStatement Throw{}     = S.empty
finalStatement s@Block{}   = finalStatement (s ^?! SL.body)
finalStatement s@Try{}     = S.fromList [toTryExitNode s , toCatchExitNode s]
finalStatement s@Fork{}    = finalInvocationFork (s ^?! SL.invocation)
finalStatement s@Seq{}     = case s ^?! SL.stat1 of 
                                Break{}    -> S.empty
                                Continue{} -> S.empty
                                Return{}   -> S.empty
                                Throw{}    -> S.empty
                                _          -> finalStatement (s ^?! SL.stat2)
finalStatement s           = S.singleton (toNode s)

fallthrough :: Statement -> CFGNodes
fallthrough s@Break{}    = S.singleton (toNode s)
fallthrough s@Continue{} = S.singleton (toNode s)
fallthrough s@Return{}   = S.singleton (toNode s)
fallthrough s@Block{}    = fallthrough (s ^?! SL.body)
fallthrough s@Ite{}      = fallthrough (s ^?! SL.trueBody) `S.union` fallthrough (s ^?! SL.falseBody)
fallthrough s@While{}    = S.filter (not . isContinueOrBreakN) (fallthrough (s ^?! SL.body))
fallthrough s@Seq{}      = fallthrough (s ^?! SL.stat1) `S.union` fallthrough (s ^?! SL.stat2)
fallthrough _            = S.empty

constructStatement :: Members [Error ErrorMessage, State ControlFlowGraph] r 
    => DeclarationMember -> Statement -> Sem r ()
constructStatement member s
    = case s of
        Assign{_lhs,_rhs} -> 
            case _rhs of
                RhsCall{_invocation} 
                    -> insertStatN s
                    <* insertCallN (Just _lhs) _invocation
                    <* insertE (initStatement s) (initInvocationCall (Just _lhs) _invocation)
                _ -> insertStatN s
        Call{_invocation} -> do
            insertStatN s 
            insertCallN Nothing _invocation
            insertE (initStatement s) (initInvocationCall Nothing _invocation)
        Ite{_trueBody, _falseBody} -> do
            insertStatN s 
            constructStatement member _trueBody 
            constructStatement member _falseBody
            insertE (initStatement s) (initStatement _trueBody)
            insertE (initStatement s) (initStatement _falseBody)
        While{_guard, _body} -> do
            insertStatN s 
            constructStatement member _body
            insertE (initStatement s) (initStatement _body)
            insertEs (finalStatement _body) (initStatement s)
            insertEs (S.filter isContinueN (fallthrough _body)) (initStatement s)
        Throw{} -> do
            insertStatN s 
            insertE (initStatement s) (-1, ExceptionalNode)
        Try{_tryBody, _catchBody} -> do
            let (tryEntryNode, tryExitNode)   = (toTryEntryNode s, toTryExitNode s)
            let (tryCatchNode, catchExitNode) = (toCatchEntryNode s, toCatchExitNode s)
            insertNs $ S.fromList [tryEntryNode, tryExitNode, tryCatchNode, catchExitNode]
            constructStatement member _tryBody
            constructStatement member _catchBody
            insertE tryEntryNode (initStatement _tryBody)
            insertEs (finalStatement _tryBody) tryExitNode
            insertE tryCatchNode (initStatement _catchBody)
            insertEs (finalStatement _catchBody) catchExitNode
        Block{_body} -> 
            constructStatement member _body
        Fork{_invocation} -> do
            insertStatN s 
            insertForkN _invocation
            insertE (initStatement s) (initInvocationFork _invocation)                    
        Seq{_stat1, _stat2} -> 
            case _stat1 of
                Continue{}  
                    -> constructStatement member _stat1 
                    <* constructStatement member _stat2 
                Break{} 
                    -> constructStatement member _stat1 
                    <* constructStatement member _stat2 
                Return{} 
                    -> constructStatement member _stat1 
                    <* constructStatement member _stat2
                Throw{}
                    -> constructStatement member _stat1 
                    <* constructStatement member _stat2
                While{_guard, _body}
                    -> constructStatement member _stat1 
                    <* constructStatement member _stat2 
                    <* insertE (initStatement _stat1) (initStatement _stat2)
                    <* insertEs (S.filter isBreakN (fallthrough _body)) (initStatement _stat2)
                _   -> constructStatement member _stat1 
                    <* constructStatement member _stat2
                    <* insertEs (finalStatement _stat1) (initStatement _stat2)
        _   -> insertStatN s

toNode :: Statement -> CFGNode
toNode s = (s ^. SL.label, StatNode s)

initInvocationCall :: Maybe Lhs -> Invocation -> CFGNode
initInvocationCall = toCallNode

finalInvocationCall :: Maybe Lhs -> Invocation -> CFGNodes
finalInvocationCall lhs invocation = S.singleton (toCallNode lhs invocation) 

toTryEntryNode :: Statement -> CFGNode
toTryEntryNode s = (s ^?! SL.label, TryEntry (s ^?! SL.label3))

toTryExitNode :: Statement -> CFGNode
toTryExitNode s = (s ^?! SL.label2, TryExit)

toCatchEntryNode :: Statement -> CFGNode
toCatchEntryNode s = (s ^?! SL.label3, CatchEntry)

toCatchExitNode :: Statement -> CFGNode
toCatchExitNode s = (s ^?! SL.label4, CatchExit)

toCallNode :: Maybe Lhs -> Invocation -> CFGNode
toCallNode lhs invocation 
    = (invocation ^. SL.label, CallNode memberEntry member thisParam arguments lhs)
    where
        arguments   = invocation ^. SL.arguments
        className   = invocation ^. SL.resolved ^?! _Just ^. _1 ^. SL.name
        member      = invocation ^. SL.resolved ^?! _Just ^. _2
        memberEntry = member ^?! SL.labels ^. _1
        thisParam    
            | Just False <- member ^? SL.isStatic 
                = Just (refType' className, invocation ^?! SL.lhs)
            | otherwise                           
                = Nothing

initInvocationFork :: Invocation -> CFGNode
initInvocationFork = toForkNode

finalInvocationFork :: Invocation -> CFGNodes
finalInvocationFork invocation = S.singleton (toForkNode invocation) 

toForkNode :: Invocation -> CFGNode
toForkNode invocation 
    = (invocation ^. SL.label, ForkNode memberEntry member arguments)
    where
        arguments   = invocation ^. SL.arguments
        member      = invocation ^. SL.resolved ^?! _Just ^. _2
        memberEntry = member ^?! SL.labels ^. _1

insertEs :: Member (State ControlFlowGraph) r 
    => CFGNodes -> CFGNode -> Sem r ()
insertEs froms to = mapM_ (`insertE` to) froms

insertE :: Member (State ControlFlowGraph) r 
    => CFGNode -> CFGNode -> Sem r ()
insertE (from, _) (to, _) = modify (insEdge (from, to, ()))

insertStatN :: Member (State ControlFlowGraph) r 
    => Statement -> Sem r ()
insertStatN stat = modify (insNode (toNode stat))

insertCallN :: Member (State ControlFlowGraph) r 
    => Maybe Lhs -> Invocation -> Sem r ()
insertCallN lhs invocation = modify (insNode (toCallNode lhs invocation))

insertForkN :: Member (State ControlFlowGraph) r 
    => Invocation -> Sem r ()
insertForkN invocation = modify (insNode (toForkNode invocation))

insertNs :: Member (State ControlFlowGraph) r 
    => CFGNodes -> Sem r ()
insertNs = mapM_ insertN

insertN :: Member (State ControlFlowGraph) r 
    => CFGNode -> Sem r ()
insertN node = modify (insNode node)

insertMemberN :: Member (State ControlFlowGraph) r 
    => SymbolTableEntry -> Sem r ()
insertMemberN entry 
    = modify ( insNode (lExit , memberExitNodeValue entry)
             . insNode (lEntry, memberEntryNodeValue entry))
    where
        (lEntry, lExit) = getMember entry ^?! SL.labels

insertExceptionalN :: Member (State ControlFlowGraph) r => Sem r ()
insertExceptionalN = modify (insNode (-1, ExceptionalNode))

memberEntryNodeValue :: SymbolTableEntry -> CFGNodeValue
memberEntryNodeValue entry
    = MemberEntry (typeOf method) (declaration ^. SL.name) (method ^?! SL.name) parameters
    where
        declaration = getDeclaration entry
        method      = getMember entry
        parameters  = map (\ param -> typeOf (param ^?! SL.ty)) (method ^?! SL.params)

memberExitNodeValue :: SymbolTableEntry -> CFGNodeValue
memberExitNodeValue entry 
    = MemberExit (typeOf method) (declaration ^. SL.name) (method ^?! SL.name) parameters
    where
        declaration = getDeclaration entry
        method      = getMember entry
        parameters  = map (\ param -> typeOf (param ^?! SL.ty)) (method ^?! SL.params)

isBreakN, isContinueN, isContinueOrBreakN, isReturnN :: CFGNode -> Bool
isContinueOrBreakN node            = isBreakN node || isContinueN node 
isBreakN           (_, StatNode s) = SL._Break `is` s
isBreakN           (_, _)          = False
isContinueN        (_, StatNode s) = SL._Continue `is` s
isContinueN        (_, _)          = False
isReturnN          (_, StatNode s) = SL._Return `is` s
isReturnN          (_, _)          = False
