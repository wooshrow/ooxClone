module Control.Monad.Extra where

import Control.Monad

whenM :: Monad m => m Bool -> m () -> m ()
whenM condition op = do
    condition' <- condition
    when condition' op

foldM' :: Monad m => (a -> b -> m a) -> a -> [b] -> m a
foldM' _ z []     = return z
foldM' f z (x:xs) = do
    z' <- f z x
    z' `seq` foldM' f z' xs