module Control.Monad.Extra where

import Control.Monad
import Data.Bifunctor

whenM :: Monad m => m Bool -> m () -> m ()
whenM condition op = do
    condition' <- condition
    when condition' op

whenJust :: Monad m => (a -> m ()) -> Maybe a -> m ()
whenJust = maybe (return ())

foldM' :: Monad m => (a -> b -> m a) -> a -> [b] -> m a
foldM' _ z []     = return z
foldM' f z (x:xs) = do
    z' <- f z x
    z' `seq` foldM' f z' xs

concatMapM :: Monad m => (a -> m [b]) -> [a] -> m [b]
concatMapM _ []     = return []
concatMapM f (x:xs) = do
    x'  <- f x
    xs' <- concatMapM f xs
    return $ x' ++ xs'

concatForM :: (Traversable t, Monad m) => t a -> (a -> m [b]) -> m [b]
concatForM t f = concat <$> forM t f

mapAccumM :: Monad m => (a -> b -> m (a, c)) -> a -> [b] -> m (a, [c])
mapAccumM _ s []     = return (s, [])
mapAccumM f s (x:xs) = do
    (s', x') <- f s x
    second (x':) <$> mapAccumM f s' xs
    