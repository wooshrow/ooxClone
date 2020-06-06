module Polysemy.Cache ( 
      Cache(..)
    , store
    , contains
    , whenContains
    , unlessContains
    , underCache
    , runCache
    , evalCache
) where

import qualified Data.HashSet                  as H
import           Data.Hashable
import           Control.Monad
import           Polysemy
import           Polysemy.Internal.Combinators

data Cache c m a where
    Store    :: c -> Cache c m ()
    Contains :: c -> Cache c m Bool

makeSem ''Cache

whenContains :: Member (Cache c) r => c -> Sem r () -> Sem r ()
whenContains value op = do
    c <- contains value
    when c op

unlessContains :: Member (Cache c) r => c -> Sem r () -> Sem r ()
unlessContains value op = do
    c <- contains value
    unless c op

underCache :: Member (Cache c) r => c -> Sem r () -> Sem r ()
underCache value f = do
    condition <- contains value
    if condition then return () else f >> store value

runCache :: (Eq c, Hashable c) => Sem (Cache c ': r) a -> Sem r (H.HashSet c, a)
runCache = (stateful $ \case
    Store v    -> \ c -> pure (v `H.insert` c, ())
    Contains v -> \ c -> pure (c, v `H.member` c)) H.empty

evalCache :: (Eq c, Hashable c) => Sem (Cache c ': r) a -> Sem r a
evalCache = fmap snd . runCache
