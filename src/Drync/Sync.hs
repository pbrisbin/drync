module Drync.Sync
    ( sync
    , syncFile
    , syncDirectory
    ) where

import Drync.Missing
import Drync.Options
import Drync.System
import Drync.Transfer

import Control.Monad (forM_, when)
import Data.List (partition)
import Data.Time (UTCTime, diffUTCTime)
import Network.Google.Drive
import System.Directory (getModificationTime)
import System.FilePath ((</>))
import System.FilePath.Glob (match)

sync :: Options -> FilePath -> File -> Api ()
sync options parent file = do
    let local = parent </> localPath file

    if isFolder file
        then syncDirectory options local file
        else syncFile options local file

syncFile :: Options -> FilePath -> File -> Api ()
syncFile options fp file = do
    modified <- liftIO $ getModificationTime fp
    let rmodified = fileModified $ fileData file

    when (different modified rmodified) $
        if modified > rmodified
            then upload options fp file
            else download options file fp

  where
    -- Downloading or uploading results in a small difference in modification
    -- times. We should ignore such differences so as to not continually
    -- re-sync files back and forth.
    different :: UTCTime -> UTCTime -> Bool
    different x = (> 30) . abs . diffUTCTime x

syncDirectory :: Options -> FilePath -> File -> Api ()
syncDirectory options fp file = do
    files <- listVisibleContents file
    paths <- liftIO $ getVisibleDirectoryContents fp

    let (locals, both, remotes) = categorize paths files

    forM_ (filter include locals) $ missingRemote options file . (fp </>)
    forM_ (filter (include . localPath) remotes) $ missingLocal options fp
    forM_ (filter (include . localPath) both) $ sync options fp

  where
    include :: FilePath -> Bool
    include path = not $ any (`match` path) $ oExcludes options

categorize :: [FilePath] -> [File] -> ([FilePath], [File], [File])
categorize paths files = (paths', both, files')
  where
    paths' = filter (`notElem` (map localPath both)) paths
    (both, files') = partition ((`elem` paths) . localPath) files
