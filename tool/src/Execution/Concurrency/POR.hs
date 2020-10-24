module Execution.Concurrency.POR where

import           Prelude hiding ((<>))
import qualified Data.Set                     as S
import           Data.Maybe
import           Control.Monad
import           Control.Lens
import           Execution.Concurrency.Thread
import           Execution.Memory.Heap
import           Execution.State
import           Execution.State.Thread
import           Analysis.CFA.CFG
import           Text.Pretty
import           Language.Syntax
import qualified Language.Syntax.Lenses       as SL
import           Language.Syntax.Fold

type InterleavingConstraints = [InterleavingConstraint]

data InterleavingConstraint
    = IndependentConstraint    CFGContext CFGContext
    | NotIndependentConstraint CFGContext CFGContext
    deriving (Show, Eq, Ord)

instance Pretty [InterleavingConstraint] where
    pretty = commas

instance Pretty InterleavingConstraint where
    pretty (IndependentConstraint (_, v, _, _) (_, w, _, _)) 
        = pretty v <> text "~" <> pretty w
    pretty (NotIndependentConstraint (_, v, _, _) (_, w, _, _)) 
        = pretty v <> text "!~" <> pretty w

type ReadWriteSet = (S.Set Reference, S.Set Reference)

-- | Returns the Set of Threads that must be interleaved.
por :: [Thread] -> Engine r InterleavingConstraints
por [_]     = return []
por threads = do
    let pairs = [(x, y) | let list = threads, x <- list, y <- list, x < y]
    foldM construct [] pairs
    where construct :: InterleavingConstraints -> (Thread, Thread) -> Engine r InterleavingConstraints
          construct acc pair@(thread1, thread2) = do
            isIndep <- isIndependent pair
            if isIndep 
                then return (IndependentConstraint (thread1 ^. pc) (thread2 ^. pc) : acc)
                else return (NotIndependentConstraint (thread1 ^. pc) (thread2 ^. pc) : acc)

isIndependent :: (Thread, Thread) -> Engine r Bool
isIndependent (thread1, thread2) = do
    (wT1, rT1) <- dependentOperationsOfT thread1
    (wT2, rT2) <- dependentOperationsOfT thread2
    return $ S.disjoint wT1 wT2 && S.disjoint rT1 wT2 && S.disjoint rT2 wT1

-- | Returns the reads and writes of the current thread.
dependentOperationsOfT :: Thread -> Engine r ReadWriteSet
dependentOperationsOfT thread = dependentOperationsOfN thread (thread ^. pc)

-- | Returns the reads and writes of the current program counter.
dependentOperationsOfN :: Thread -> CFGContext -> Engine r ReadWriteSet
dependentOperationsOfN thread (_, _, StatNode stat, _) 
    = dependentOperationsOfS thread stat
dependentOperationsOfN thread (_, _, CallNode _ _ Nothing arguments _, _) 
    = (,S.empty) . S.unions <$> mapM (dependentOperationsOfE thread) arguments
--dependentOperationsOfN thread (_, _, CallNode _ _ (Just this) arguments _, _) 
--    = (,S.empty) . S.unions <$> mapM (dependentOperationsOfE thread) arguments
dependentOperationsOfN thread (_, _, ForkNode _ _ arguments, _) 
    = (,S.empty) . S.unions <$> mapM (dependentOperationsOfE thread) arguments
dependentOperationsOfN _ _                        
    = return (S.empty, S.empty)

-- | Returns the reads and writes of the current statement.
dependentOperationsOfS :: Thread -> Statement -> Engine r ReadWriteSet
dependentOperationsOfS thread (Assign lhs rhs _ _) = (\ l r -> (l,r)) <$> dependentOperationsOfLhs thread lhs <*> dependentOperationsOfRhs thread rhs
dependentOperationsOfS thread (Assert ass _ _)     = (,S.empty) <$> dependentOperationsOfE thread ass
dependentOperationsOfS thread (Assume ass _ _)     = (,S.empty) <$> dependentOperationsOfE thread ass
dependentOperationsOfS thread (Return expr _ _)    = (,S.empty) <$> maybe (return S.empty) (dependentOperationsOfE thread) expr
dependentOperationsOfS thread (Lock var _ _)       = (\ refs -> (refs, refs)) <$> getReferences thread var
dependentOperationsOfS thread (Unlock var _ _)     = (\ refs -> (refs, refs)) <$> getReferences thread var
dependentOperationsOfS _      _                    = return (S.empty, S.empty)

dependentOperationsOfLhs :: Thread -> Lhs -> Engine r (S.Set Reference)
dependentOperationsOfLhs _      LhsVar{}               = return S.empty
dependentOperationsOfLhs thread (LhsField var _ _ _ _) = getReferences thread var
dependentOperationsOfLhs thread (LhsElem var _ _ _)    = getReferences thread var

dependentOperationsOfRhs :: Thread -> Rhs -> Engine r (S.Set Reference)
dependentOperationsOfRhs thread (RhsExpression value _ _) = dependentOperationsOfE thread value
dependentOperationsOfRhs thread (RhsField var _ _ _)      = getReferences thread (var ^?! SL.var)
dependentOperationsOfRhs thread (RhsElem var _ _ _)       = getReferences thread (var ^?! SL.var)
dependentOperationsOfRhs thread (RhsCall inv _ _)         = dependentOperationsOfI thread inv
dependentOperationsOfRhs thread (RhsArray _ sizes _ _)    = S.unions <$> mapM (dependentOperationsOfE thread) sizes

dependentOperationsOfI :: Thread -> Invocation -> Engine r (S.Set Reference)
dependentOperationsOfI thread inv 
    = S.unions <$> mapM (dependentOperationsOfE thread) (inv ^. SL.arguments)

dependentOperationsOfE :: Thread -> Expression -> Engine r (S.Set Reference)
dependentOperationsOfE thread = foldExpression algebra
    where
        algebra = monoidMExpressionAlgebra -- TODO: quantifiers (- and var and ref)
            { fSizeOf = \ var _ _ -> getReferences thread var }

getReferences :: Thread -> Identifier -> Engine r (S.Set Reference)
getReferences thread var = do
    ref <- readVar thread var
    processRef ref
        (return . S.singleton)
        (\ (SymbolicRef symRef _ _) -> do
            aliases <- fromJust <$> getAliasesWithoutNull symRef
            return $ S.map (^?! SL.ref) aliases)
        (return S.empty)
