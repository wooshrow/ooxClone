module Logger(
      Trace
    , inform
    , debug
) where

import GHC.Stack
import Control.Monad
import Polysemy
import Polysemy.Trace
import Polysemy.Reader
import Data.Time.LocalTime
import Data.Configuration

inform :: (HasConfiguration a, Members [Reader a, Trace, Embed IO] r) => String -> Sem r ()
inform message = do
    Configuration{logLevel} <- configuration <$> ask
    when (logLevel >= 1) $
        construct message

debug :: (HasCallStack, HasConfiguration a, Members [Reader a, Trace, Embed IO] r) => String -> Sem r ()
debug message = do
    Configuration{logLevel} <- configuration <$> ask
    when (logLevel >= 2) $
        construct message

construct :: Members [Trace, Embed IO] r => String -> Sem r ()
construct message = do
    time <- showZonedTime <$> embed getZonedTime
    trace ("[" ++ time ++ "] " ++ message)

showZonedTime :: ZonedTime -> String
showZonedTime time = show hours ++ ":" ++ show minutes ++ ":" ++ take 2 (show seconds)
    where
        (TimeOfDay hours minutes seconds) = localTimeOfDay (zonedTimeToLocalTime time)