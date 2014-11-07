module Data.Conduit.Progress
    ( reportProgress
    , reportProgressWith
    , defaultReporter
    ) where

import Control.Monad (when)
import Control.Monad.IO.Class
import Data.Conduit (Conduit, await, yield)
import Data.Monoid ((<>))
import Data.Time (UTCTime, diffUTCTime, getCurrentTime)
import System.IO (hFlush, stdout)

type Units = Int

data Progress = Progress
    { progressStart :: !UTCTime
    , progressNow :: !UTCTime
    , progressCurrent :: !Units
    , progressTotal :: !Units
    }

type Reporter = Progress -> IO ()

reportProgress :: MonadIO m => (i -> Units) -> Units -> Units -> Conduit i m i
reportProgress = reportProgressWith defaultReporter

reportProgressWith :: MonadIO m
                   => Reporter
                   -> (i -> Units)
                   -> Units
                   -> Units
                   -> Conduit i m i
reportProgressWith reporter len total each = do
    now <- liftIO $ getCurrentTime

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
            Nothing -> return ()
            Just v -> do
                now <- liftIO $ getCurrentTime

                let p' = increment (len v) now p

                when (progressCurrent p' `mod` each == 0) $ liftIO $ reporter p'

                yield v

                loop p'

    increment units now p = p
        { progressNow = now
        , progressCurrent = progressCurrent p + units
        }

defaultReporter :: Reporter
defaultReporter p = rewrite $ bar p 50 <>
    constrain 15 (show $ speed p) <> " per second" <>
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
speed :: Progress -> Int
speed p@Progress{..} = progressCurrent `divide` elapsed p

-- | Seconds elapsed
elapsed :: Progress -> Int
elapsed Progress{..} = round $ diffUTCTime progressNow progressStart

-- | Seconds remaining
remaining :: Progress -> Int
remaining p@Progress{..} = left `divide` speed p
  where
    left :: Int
    left = progressTotal - progressCurrent

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

divide :: (Integral a, Integral b) => a -> b -> Int
divide a b = round $ (fromIntegral a :: Double) / (fromIntegral b :: Double)

constrain :: Int -> String -> String
constrain n s
    | length s > n = pad n ""
    | otherwise = pad n $ take n s

pad :: Int -> String -> String
pad n v = replicate (n - length v) ' ' <> v
