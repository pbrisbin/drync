module Drync.Sync
    ( Sync(..)
    , sync
    ) where

import Control.Applicative ((<$>), (<*>))
import Control.Monad (void, when)
import Data.Monoid ((<>))
import Data.Text (Text)
import Data.Time (UTCTime, diffUTCTime)
import Network.Google.Drive.Api
import Network.Google.Drive.File
import Network.Google.Drive.Search
import Network.Google.OAuth2
import System.Directory
    ( doesDirectoryExist
    , doesFileExist
    , getDirectoryContents
    , getModificationTime
    )
import System.FilePath ((</>), takeFileName)

import qualified Data.Text as T

data Sync
    = Sync FilePath File
    | SyncFile FilePath File
    | SyncDirectory FilePath File
    | Upload FilePath File
    | Download File FilePath

sync :: OAuth2Token -> FilePath -> Text -> IO ()
sync token from to = runApi token $ do
    files <- getFiles $ TitleEq to `And` ParentEq "root"

    case files of
        (file:_) -> executeSync (SyncDirectory from file)
        _ -> logApiErr $ T.unpack to <> " does not exist"

executeSync :: Sync -> Api ()
executeSync (Sync path file) = do
    isFileDirectory <- liftIO $ (,)
        <$> doesFileExist path
        <*> doesDirectoryExist path

    case isFileDirectory of
        (True, _) -> executeSync $ SyncFile path file
        (_, True) -> executeSync $ SyncDirectory path file
        _ -> logApiErr $ path <> " does not exist"

executeSync (SyncFile path file) = do
    localModified <- liftIO $ getModificationTime path

    when (different localModified $ fileModified file) $
        if localModified > fileModified file
            then updateFile path file
            else downloadFile file path

  where
    -- Downloading or uploading results in a small difference in modification
    -- times. We should ignore such differences so as to not continually
    -- re-sync files back and forth.
    different :: UTCTime -> UTCTime -> Bool
    different x = (> 30) . abs . diffUTCTime x

executeSync (SyncDirectory path file) = do
    files <- getFiles $ ParentEq (fileId file)
    paths <- liftIO $ getVisibleDirectoryContents path

    -- probably inefficient but hopefuly these are small enough lists
    let both = filter ((`elem` paths) . T.unpack . fileTitle) files
        local = filter ((`notElem` (map fileTitle both)) . T.pack) paths
        remote = filter (`notElem` both) files

    mapM_ (syncEach path) both
    mapM_ (uploadEach file . (path </>)) local
    mapM_ (downloadEach path) remote

executeSync (Download file path) = do
    case fileDownloadUrl file of
        Just _ -> downloadFile file path
        Nothing -> do
            files <- getFiles $ ParentEq (fileId file)
            mapM_ (downloadEach path) files

executeSync (Upload path file) = do
    isDirectory <- liftIO $ doesDirectoryExist path

    if not isDirectory
        then void $ createFile path file
        else do
            files <- liftIO $ getVisibleDirectoryContents path

            let parentId = fileId file
                name = T.pack $ takeFileName path

            mfolder <- createFolder parentId name

            case mfolder of
                Nothing -> logApiErr $ "Folder " <> T.unpack name <> " not created"
                Just folder -> mapM_ (uploadEach folder . (path </>)) files

syncEach :: FilePath -> File -> Api ()
syncEach path file = executeSync $ Sync (localPath path file) file

uploadEach :: File -> FilePath -> Api ()
uploadEach file path = executeSync $ Upload path file

downloadEach :: FilePath -> File -> Api ()
downloadEach path file = executeSync $ Download file (localPath path file)

getVisibleDirectoryContents :: FilePath -> IO [FilePath]
getVisibleDirectoryContents path = filter visible <$> getDirectoryContents path

  where
    visible :: FilePath -> Bool
    visible ('.':_) = False
    visible _ = True

localPath :: FilePath -> File -> FilePath
localPath p i = p </> T.unpack (fileTitle i)
