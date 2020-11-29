module Data.Configuration where

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
    , logLevel                :: Int
    , runBenchmark            :: Bool
    }

class HasConfiguration a where
    configuration :: a -> Configuration

instance HasConfiguration Configuration where
    configuration = id

instance HasConfiguration (Configuration, b, c) where
    configuration (c, _, _) = c
