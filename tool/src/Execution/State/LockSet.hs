module Execution.State.LockSet(
      LockSet
    , singleton
    , insert
    , remove
    , lookup
) where

import           Prelude              hiding (lookup)
import qualified Data.Map        as M
import           Execution.State.Thread (ThreadId)
import           Language.Syntax

newtype LockSet = LockSet { unLockSet :: M.Map Reference ThreadId }
    deriving (Show)

instance Semigroup LockSet where
    (LockSet a) <> (LockSet b) = LockSet (a <> b)

instance Monoid LockSet where
    mempty = LockSet M.empty

singleton :: Reference -> ThreadId -> LockSet
singleton ref = LockSet . M.singleton ref

insert :: Reference -> ThreadId -> LockSet -> LockSet
insert ref tid = LockSet . M.insert ref tid . unLockSet

remove :: Reference -> LockSet -> LockSet
remove ref = LockSet . M.delete ref . unLockSet

lookup :: Reference -> LockSet -> Maybe ThreadId
lookup ref = M.lookup ref . unLockSet