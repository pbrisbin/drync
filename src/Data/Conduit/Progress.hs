{-# LANGUAGE RecordWildCards #-}
module Data.Conduit.Progress
    ( reportProgress
    , reportProgressWith

    -- * Reporters
    , defaultReporter

    -- * Reporter helpers
    , bar
    , speed
    , roundedSpeed
    , elapsed
    , remaining
    , rewrite
    , showTime
    , constrain
    , pad
    ) where

import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Conduit (Conduit, await, yield)
import Data.Monoid ((<>))
import Data.Time (UTCTime, diffUTCTime, getCurrentTime)
import System.IO (hFlush, stdout)

data Progress = Progress
    { progressStart :: !UTCTime
    , progressNow :: !UTCTime
    , progressCurrent :: !Int
    , progressTotal :: !Int
    }

data Reporter = Reporter
    { reportEach :: Progress -> IO () -- ^ Called on the given interval
    , reportEnd :: Progress -> IO ()  -- ^ Called when the process finishes
    }

-- | See @'reportProgressWith'@ for a description of arguments
reportProgress :: MonadIO m => (i -> Int) -> Int -> Int -> Conduit i m i
reportProgress = reportProgressWith defaultReporter

reportProgressWith :: MonadIO m
                   => Reporter
                   -> (e -> Int) -- ^ How many units per element
                   -> Int        -- ^ Total units we're working toward
                   -> Int        -- ^ Report progess every this many units
                   -> Conduit e m e
reportProgressWith reporter len total each = do
    now <- liftIO getCurrentTime

    loop Progress
        { progressStart = now
        , progressNow = now
        , progressCurrent = 0
        , progressTotal = total
        }

  where
    loop p = do
        mv <- await

        case mv of
            Nothing -> do
                liftIO $ reportEnd reporter p
                return ()

            Just v -> do
                now <- liftIO getCurrentTime

                let p' = increment (len v) now p

                when (progressCurrent p' `mod` each == 0) $
                    liftIO $ reportEach reporter p'

                yield v

                loop p'

    increment units now p = p
        { progressNow = now
        , progressCurrent = progressCurrent p + units
        }

-- | A single, self-overwriting line:
--
-- > [###      ]   42 per second,   2m3s remaining
--
defaultReporter :: Reporter
defaultReporter = Reporter
    { reportEach = rewrite . report
    , reportEnd = putStrLn . report
    }
  where
    report p =
        bar p 50 <>
        constrain 15 (show $ roundedSpeed p) <> " per second" <>
        constrain 10 (showTime $ remaining p) <> " remaining"

rewrite :: String -> IO ()
rewrite s = do
    putStr $ s <> "\r"
    hFlush stdout

-- | @[###       ]@
bar :: Progress -> Int -> String
bar Progress{..} width = "[" <> bars <> "]"
  where
    bars :: String
    bars = replicate count '#' <> replicate (width - count) ' '

    count :: Int
    count = round $ percent * fromIntegral width

    percent :: Double
    percent = fromIntegral progressCurrent / fromIntegral progressTotal

-- | Units per second
speed :: Progress -> Double
speed p@Progress{..} = fromIntegral progressCurrent / elapsed p

roundedSpeed :: Progress -> Int
roundedSpeed p = round $ speed p

-- | Seconds elapsed
elapsed :: Progress -> Double
elapsed Progress{..} = realToFrac $ diffUTCTime progressNow progressStart

-- | Seconds remaining
remaining :: Progress -> Int
remaining p@Progress{..} = round $ fromIntegral left / speed p
  where
    left :: Int
    left = progressTotal - progressCurrent

-- | Convert seconds to (e.g.) "1m12m54s"
showTime :: Int -> String
showTime seconds
    | seconds >= 3600 = showHours
    | seconds >= 60 = showMinutes
    | otherwise = show seconds <> "s"
  where
    showHours =
        let (h,s) = seconds `divMod` 3600
        in show h <> "h" <> showTime s

    showMinutes =
        let (m, s) = seconds `divMod` 60
        in show m <> "m" <> showTime s

-- | Pad a value, and prevent overflow
constrain :: Int -> String -> String
constrain n s
    | length s > n = pad n ""
    | otherwise = pad n $ take n s

pad :: Int -> String -> String
pad n v = replicate (n - length v) ' ' <> v
