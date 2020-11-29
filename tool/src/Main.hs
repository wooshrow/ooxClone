module Main where

import Polysemy
import Polysemy.State
import Polysemy.Error
import Polysemy.Reader
import Polysemy.Trace
import Control.Monad
import Options.Applicative
import Data.Time.Clock
import Data.Error
import Data.Configuration
import Data.Statistics
import Text.Pretty          (toString)
import Parsing.Phase
import Analysis.Phase
import Execution.Phase
import Verification.Result

allPhases :: Members [Reader Configuration, State Statistics, Error ErrorMessage, Embed IO] r 
    => Sem r VerificationResult
allPhases = traceToIO (parsingPhase >>= analysisPhase >>= uncurry executionPhase)

executeOnce :: Configuration -> IO (VerificationResult, Statistics)
executeOnce config = do
    startTime <- getCurrentTime
    (statistics, result) <- runM (runState initialStatistics (runReader config (runError allPhases)))
    endTime <- getCurrentTime
    let runtime = endTime `diffUTCTime` startTime
    putStrLn "Statistics"
    case result of
        Left  e -> putStrLn $ "\t Error result:\t\t" ++ toString e
        Right r -> putStrLn $ "\t Final result:\t\t" ++ toString r
    putStrLn $ "\t Total time:\t\t" ++ show runtime
    putStrLn $ "\t Total #branches:\t" ++ show (_numberOfBranches statistics)
    putStrLn $ "\t Total #prunes:\t\t" ++ show (_numberOfPrunes statistics)
    putStrLn $ "\t Total #complete paths:\t" ++ show (_numberOfCompletePaths statistics)
    putStrLn $ "\t Total #verifications:\t" ++ show (_numberOfVerifications statistics)
    putStrLn $ "\t Total #locally solved:\t" ++ show (_numberOfLocalSolves statistics)
    putStrLn $ "\t Total #Z3 invocations:\t" ++ show (_numberOfZ3Invocations statistics)
    putStrLn $ "\t Maximum #forks:\t" ++ show (_maximumNumberOfThreads statistics)
    return (either (const (error "error result")) id result, statistics{_totalRuntime = runtime})

execute :: Configuration -> IO (VerificationResult, [Statistics])
execute config@Configuration{runBenchmark} 
    | runBenchmark = do
        results <- replicateM 2 (executeOnce config)
        putStrLn "Final Statistics"
        let runtime = (averageRuntime . map (_totalRuntime . snd)) results
        putStrLn $ "\t Average runtime:\t" ++ show runtime
        return (head (map fst results), map snd results)
    | otherwise = do
        (result, statistic) <- executeOnce config
        return (result, [statistic])

averageRuntime :: [NominalDiffTime] -> NominalDiffTime
averageRuntime xs = fromRational (sum (map toRational xs) / toRational (length xs))

--------------------------------------------------------------------------------
-- Argument parsing and main
--------------------------------------------------------------------------------

parseConfiguration :: Parser Configuration
parseConfiguration = Configuration 
                <$> strArgument 
                    ( help "The OOX source file to verify" )
                <*> option auto
                    (  help "The maximum program path depth"
                    <> long "path-depth"
                    <> short 'k'
                    <> metavar "INT"
                    <> value 10 )
                <*> strOption
                    (  help  "The OOX function to verify"
                    <> long  "function"
                    <> short 'f'
                    <> metavar "STRING"
                    <> value "main" )
                <*> flag True False
                    (  help "Do not verify the post-conditions of methods and constructors"
                    <> long "no-ensures" )
                <*> flag True False
                    (  help "Do not verify the exceptional post-conditions of methods and constructors"
                    <> long "no-exceptional" )
                <*> flag True False
                    (  help "Do not verify the pre-conditions of methods and constructors"
                    <> long "no-requires" )
                <*> flag True False
                    ( help "Do not verify the null case of symbolic reference typed arguments"
                    <> long "no-symbolic-null" )
                <*> flag True False
                    ( help "Do not verify any alias case of symbolic reference typed arguments"
                    <> long "no-symbolic-alias")
                <*> option auto
                    ( help "Maximum symbolic array size"
                    <> long "symbolic-array-size"
                    <> metavar "INT"
                    <> value 3 )
                <*> flag True False
                    ( help "Do not use a local cache for formulas sent to the verification back-end"
                    <> long "no-cache")
                <*> flag True False
                    ( help "Do not apply apply partial order reduction"
                    <> long "no-por")  
                <*> flag True False
                    ( help "Do not apply local solver before querying Z3"
                    <> long "no-local-solver")  
                <*> flag True False
                    ( help "Do not randomly select the next interleaving to explore next"
                    <> long "no-random-interleaving")
                <*> option auto
                    (  help "Information printing level"
                    <> long "inform"
                    <> short 'i'
                    <> metavar "0 (Nothing), 1 (Informative) or 2 (Debug)"
                    <> value 1 )
                <*> flag False True
                    ( help "Execute as a benchmark of N runs"
                    <> long "benchmark" )

parserInfo :: ParserInfo Configuration
parserInfo = info (parseConfiguration <**> helper)
              (fullDesc <> header "A Symbolic Execution Tool for OOX Programs")

main :: IO ()
main = execParser parserInfo >>= void . execute