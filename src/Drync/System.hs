module Drync.System
    ( getFileSize
    , getVisibleDirectoryContents
    , setModificationTime
    ) where

import Control.Applicative ((<$>))
import Data.Time (UTCTime, getCurrentTime)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import System.Directory (getDirectoryContents)
import System.IO (IOMode(..), hFileSize, withFile)
import System.Posix (EpochTime, setFileTimes)

getFileSize :: Integral a => FilePath -> IO a
getFileSize fp = fromIntegral <$> withFile fp ReadMode hFileSize

getVisibleDirectoryContents :: FilePath -> IO [FilePath]
getVisibleDirectoryContents path = filter visible <$> getDirectoryContents path

  where
    visible :: FilePath -> Bool
    visible ('.':_) = False
    visible _ = True

-- sets mtime to time, sets atime to now
setModificationTime :: FilePath -> UTCTime -> IO ()
setModificationTime fp t = do
    now <- getCurrentTime

    setFileTimes fp (toEpoch now) (toEpoch t)
  where
    toEpoch :: UTCTime -> EpochTime
    toEpoch = fromIntegral . toSecs

    toSecs :: UTCTime -> Int
    toSecs = round . utcTimeToPOSIXSeconds
