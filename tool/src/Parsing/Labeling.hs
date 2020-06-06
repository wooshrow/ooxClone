module Parsing.Labeling (
    labelize
) where

import           Polysemy      
import           Polysemy.State
import           Control.Lens
import           Language.Syntax
import qualified Language.Syntax.Lenses as SL
import           Language.Syntax.Fold

labelize :: CompilationUnit -> Sem r CompilationUnit
labelize = evalState 1 . labelizeCompilationUnit

--------------------------------------------------------------------------------
-- Labeling management
--------------------------------------------------------------------------------

type LabelingEffects r a = Member (State Label) r => Sem r a

nextLabel :: LabelingEffects r Label
nextLabel = do
    l <- get
    put (l + 1)
    return l

--------------------------------------------------------------------------------
-- Labeling
--------------------------------------------------------------------------------

labelizeCompilationUnit :: CompilationUnit -> LabelingEffects r CompilationUnit
labelizeCompilationUnit p@CompilationUnit{} = do
    members' <- mapM labelizeDeclaration (p ^. SL.members)
    return (p & SL.members .~ members')

labelizeDeclaration :: Declaration -> LabelingEffects r Declaration
labelizeDeclaration c@Class{} = do
    members' <- mapM labelizeMember (c ^. SL.members)
    return (c & SL.members .~ members')

labelizeMember :: DeclarationMember -> LabelingEffects r DeclarationMember
labelizeMember c@Constructor{} = do
    entryLabel <- nextLabel
    body' <- labelizeStat (c ^?! SL.body)
    exitLabel <- nextLabel
    return $ c & (SL.body .~ body') & (SL.labels .~ (entryLabel, exitLabel))
labelizeMember m@Method{} = do
    entryLabel <- nextLabel
    body' <- labelizeStat (m ^?! SL.body)
    exitLabel <- nextLabel
    return $ m & (SL.body .~ body') & (SL.labels .~ (entryLabel, exitLabel))
labelizeMember f@Field{} = return f

labelizeStat :: Statement -> LabelingEffects r Statement
labelizeStat = foldStatement labelingAlg

labelingAlg :: Member (State Label) r => StatementAlgebra (Sem r Statement)
labelingAlg = 
    ( \ ty name _ s -> Declare ty name <$> nextLabel <*> pure s
    , \ lhs rhs _ s -> do
        l <- nextLabel
        Assign lhs <$> labelingRhs rhs <*> pure l <*> pure s
    , \ inv _ s -> do
        l <- nextLabel
        Call <$> labelizeInv inv <*> pure l <*> pure s
    , \ _ s -> Skip <$> nextLabel <*> pure s
    , \ e _ s -> Assert e <$> nextLabel <*> pure s
    , \ e _ s -> Assume e <$> nextLabel <*> pure s
    , \ guard body _ s -> (\ l body' -> While guard body' l s) <$> nextLabel <*> body 
    , \ guard tBody fBody _ s -> (\ l tB fB -> Ite guard tB fB l s) <$> nextLabel <*> tBody <*> fBody
    , \ _ s -> Continue <$> nextLabel <*> pure s
    , \ _ s -> Break <$> nextLabel <*> pure s
    , \ e _ s -> Return e <$> nextLabel <*> pure s
    , \ m _ s -> Throw m <$> nextLabel <*> pure s
    , \ tBody cBody _ _ _ _ s -> do
        l1 <- nextLabel
        tB <- tBody
        l2 <- nextLabel
        l3 <- nextLabel
        cB <- cBody
        l4 <- nextLabel
        return $ Try tB cB l1 l2 l3 l4 s
    , \ body _ s -> (\ l b -> Block b l s) <$> nextLabel <*> body
    , \ var _ s -> Lock var <$> nextLabel <*> pure s
    , \ var _ s -> Unlock var <$> nextLabel <*> pure s
    , \ inv _ s -> do
        l <- nextLabel
        Fork <$> labelizeInv inv <*> pure l <*> pure s
    , \ _ s -> Join <$> nextLabel <*> pure s
    , \ s1 s2 _ s -> do
        l'  <- nextLabel
        s1' <- s1
        s2' <- s2
        case s1' of
            Seq a b l x -> pure $ Seq a (Seq b s2' l' s) l x
            _           -> pure $ Seq s1' s2' l' s)
            
labelingRhs :: Rhs -> LabelingEffects r Rhs
labelingRhs rhs@RhsCall{} = do
    inv' <- labelizeInv (rhs ^?! SL.invocation)
    return $ rhs & (SL.invocation .~ inv')
labelingRhs rhs = return rhs

labelizeInv :: Invocation -> LabelingEffects r Invocation
labelizeInv inv = do 
    label <- nextLabel
    return $ inv & (SL.label .~ label)
