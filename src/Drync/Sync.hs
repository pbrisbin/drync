module Drync.Sync
    ( Sync(..)
    , executeSync
    ) where

import Control.Applicative ((<$>))
import Control.Monad (void)
import System.Directory -- TODO imports
import System.FilePath ((</>), takeFileName)

import qualified Data.Text as T

import Drync.Drive.Api
import Drync.Drive.Item

data Sync
    = Sync FilePath Item
    | SyncFile FilePath Item
    | SyncDirectory FilePath Item
    | Upload FilePath Item
    | Download Item FilePath

executeSync :: Sync -> Api ()
executeSync (Sync path item) = do
    isFile <- liftIO $ doesFileExist path
    isDirectory <- liftIO $ doesDirectoryExist path

    case (isFile, isDirectory) of
        (True, _) -> executeSync $ SyncFile path item
        (_, True) -> executeSync $ SyncDirectory path item
        _ -> return () -- TODO: putErr

executeSync (SyncFile path item) = do
    fileModified <- liftIO $ getModificationTime path

    if fileModified > itemModified item
        then updateFile path item
        else downloadFile item path

executeSync (SyncDirectory path item) = do
    items <- getFiles $ ParentEq (itemId item)
    files <- liftIO $ getVisibleDirectoryContents path

    -- probably inefficient but hopefuly these are small enough lists
    let both = filter ((`elem` files) . T.unpack . itemTitle) items
        local = filter ((`notElem` (map itemTitle both)) . T.pack) files
        remote = filter (`notElem` both) items

    mapM_ (syncEach path) both
    mapM_ (uploadEach item . (path </>)) local
    mapM_ (downloadEach path) remote

executeSync (Download item path) = do
    case itemDownloadUrl item of
        Just _ -> downloadFile item path
        Nothing -> do
            items <- getFiles $ ParentEq (itemId item)
            mapM_ (downloadEach path) items

executeSync (Upload path item) = do
    isDirectory <- liftIO $ doesDirectoryExist path

    if not isDirectory
        then void $ createFile path item
        else do
            files <- liftIO $ getVisibleDirectoryContents path

            let parentId = itemId item
                name = T.pack $ takeFileName path

            folder <- createFolder parentId name

            mapM_ (\f -> executeSync $ Upload (path </> f) folder) files

syncEach :: FilePath -> Item -> Api ()
syncEach path item = executeSync $ Sync (localPath path item) item

uploadEach :: Item -> FilePath -> Api ()
uploadEach item path = executeSync $ Upload path item

downloadEach :: FilePath -> Item -> Api ()
downloadEach path item = executeSync $ Download item (localPath path item)

getVisibleDirectoryContents :: FilePath -> IO [FilePath]
getVisibleDirectoryContents path = filter visible <$> getDirectoryContents path

  where
    visible :: FilePath -> Bool
    visible ('.':_) = False
    visible _ = True

localPath :: FilePath -> Item -> FilePath
localPath p i = p </> T.unpack (itemTitle i)