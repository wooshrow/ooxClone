module Data.Statistics where

import Control.Lens
import Data.Time.Clock
import Polysemy
import Polysemy.State

data Statistics = Statistics
    { _numberOfBranches             :: Int
    , _numberOfPrunes               :: Int
    , _numberOfCompletePaths        :: Int
    , _numberOfVerifications        :: Int
    , _numberOfLocalSolves          :: Int
    , _numberOfCacheHits            :: Int
    , _numberOfZ3Invocations        :: Int
    , _totalRuntime                 :: NominalDiffTime
    , _maximumNumberOfThreads       :: Int }

$(makeFieldsNoPrefix ''Statistics)

initialStatistics :: Statistics
initialStatistics = Statistics
    { _numberOfBranches             = 1 
    , _numberOfPrunes               = 0 
    , _numberOfCompletePaths        = 0
    , _numberOfVerifications        = 0
    , _numberOfLocalSolves          = 0
    , _numberOfCacheHits            = 0
    , _numberOfZ3Invocations        = 0
    , _totalRuntime                 = 0
    , _maximumNumberOfThreads       = 0 }

measureBranches :: (Member (State Statistics) r, Foldable t) => t a -> Sem r ()
measureBranches xs 
    | null xs   = return ()
    | otherwise = do
        let newBranches = length xs - 1
        modify (\ stats -> stats & (numberOfBranches +~ newBranches))

measurePrunes :: Member (State Statistics) r => Int -> Sem r ()
measurePrunes n = modify (\ stats -> stats & (numberOfPrunes +~ n))

measurePrune :: Member (State Statistics) r => Sem r ()
measurePrune = measurePrunes 1

measureFinish :: Member (State Statistics) r => Sem r ()
measureFinish = modify (\ stats -> stats & (numberOfCompletePaths +~ 1))

measureVerification :: Member (State Statistics) r => Sem r ()
measureVerification = modify (\ stats -> stats & (numberOfVerifications +~ 1))

measureLocalSolve :: Member (State Statistics) r => Sem r ()
measureLocalSolve = modify (\ stats -> stats & (numberOfLocalSolves +~ 1))

measureCacheHit :: Member (State Statistics) r => Sem r ()
measureCacheHit = modify (\ stats -> stats & (numberOfCacheHits +~ 1))

measureMaximumForks :: Member (State Statistics) r => Int -> Sem r ()
measureMaximumForks forks = modify (\ stats -> stats & maximumNumberOfThreads .~ max (_maximumNumberOfThreads stats) forks)

measureInvokeZ3 :: Member (State Statistics) r => Sem r ()
measureInvokeZ3 = modify (\ stats -> stats & (numberOfZ3Invocations +~ 1))
