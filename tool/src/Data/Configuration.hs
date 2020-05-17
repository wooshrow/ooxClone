module Data.Configuration where

import Polysemy
import Polysemy.Reader
import Control.Monad

data Configuration 
    = Configuration 
    { fileName                :: FilePath
    , maximumDepth            :: Int
    , entryPoint              :: String
    , verifyEnsures           :: Bool
    , verifyExceptional       :: Bool
    , verifyRequires          :: Bool
    , symbolicNulls           :: Bool
    , symbolicAliases         :: Bool
    , symbolicArraySize       :: Int
    , cacheFormulas           :: Bool
    , applyPOR                :: Bool
    , applyLocalSolver        :: Bool
    , applyRandomInterleaving :: Bool
    , informationLevel        :: Int
    , runBenchmark            :: Bool
    }

class HasConfiguration a where
    configuration :: a -> Configuration

instance HasConfiguration Configuration where
    configuration = id

instance HasConfiguration (Configuration, b, c) where
    configuration (c, _, _) = c

inform :: (HasConfiguration a, Members [Reader a, Embed IO] r) => String -> Sem r ()
inform message = do
    Configuration{informationLevel} <- configuration <$> ask
    when (informationLevel >= 1)
        (embed (putStrLn message))

debug :: (HasConfiguration a, Members [Reader a, Embed IO] r) => String -> Sem r ()
debug message = do
    Configuration{informationLevel} <- configuration <$> ask
    when (informationLevel >= 2)
        (embed (putStrLn message))
