module Polysemy.LocalState where

import Polysemy
import Polysemy.Internal.Combinators

data LocalState s m a where
    PutLocal :: s -> LocalState s m ()
    GetLocal :: LocalState s m s

makeSem ''LocalState

modifyLocal :: Member (LocalState s) r => (s -> s) -> Sem r ()
modifyLocal f = do
    s <- getLocal
    putLocal $! f s

runLocalState :: s -> Sem (LocalState s ': r) a -> Sem r (s, a)
runLocalState = stateful $ \case
    GetLocal   -> \ s -> pure (s, s)
    PutLocal s -> const $ pure (s, ())

evalLocalState :: s -> Sem (LocalState s ': r) a -> Sem r a
evalLocalState s = fmap snd . runLocalState s