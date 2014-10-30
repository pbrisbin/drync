module Drync.Sync
    ( Sync(..)
    , executeSync
    ) where

import Control.Applicative ((<$>))
import Control.Monad (void)
import System.Directory -- TODO imports
import System.FilePath ((</>), takeFileName)

import qualified Data.Text as T

import Drync.Token
import Drync.Drive.Api
import Drync.Drive.Item

data Sync
    = Sync FilePath Item
    | SyncFile FilePath Item
    | SyncDirectory FilePath Item
    | Upload FilePath Item
    | Download Item FilePath

executeSync :: OAuth2Tokens -> Sync -> IO ()
executeSync tokens (Sync path item) = do
    isFile <- doesFileExist path
    isDirectory <- doesDirectoryExist path

    case (isFile, isDirectory) of
        (True, _) -> executeSync tokens $ SyncFile path item
        (_, True) -> executeSync tokens $ SyncDirectory path item
        _ -> return () -- TODO: putErr

executeSync tokens (SyncFile path item) = do
    fileModified <- getModificationTime path

    if fileModified > itemModified item
        then updateFile tokens path item
        else downloadFile tokens item path

executeSync tokens (SyncDirectory path item) = do
    items <- getFiles tokens $ ParentEq (itemId item)
    files <- filter (not . hidden) <$> getDirectoryContents path

    -- probably inefficient but hopefuly these are small enough lists
    let both = filter ((`elem` files) . T.unpack . itemTitle) items
        local = filter ((`notElem` (map itemTitle both)) . T.pack) files
        remote = filter (`notElem` both) items

    mapM_ (syncEach tokens path) both
    mapM_ (uploadEach tokens item . (path </>)) local
    mapM_ (downloadEach tokens path) remote

executeSync tokens (Download item path) = do
    case itemDownloadUrl item of
        Just _ -> downloadFile tokens item path
        Nothing -> do
            items <- getFiles tokens $ ParentEq (itemId item)
            mapM_ (downloadEach tokens path) items

executeSync tokens (Upload path item) = do
    isDirectory <- doesDirectoryExist path

    if not isDirectory
        then void $ createFile tokens path item
        else do
            files <- filter (not . hidden) <$> getDirectoryContents path

            let parentId = itemId item
                name = T.pack $ takeFileName path

            folder <- createFolder tokens parentId name

            mapM_ (\f -> executeSync tokens $ Upload (path </> f) folder) files

syncEach :: OAuth2Tokens -> FilePath -> Item -> IO ()
syncEach tokens path item = executeSync tokens $ Sync (localPath path item) item

uploadEach :: OAuth2Tokens -> Item -> FilePath -> IO ()
uploadEach tokens item path = executeSync tokens $ Upload path item

downloadEach :: OAuth2Tokens -> FilePath -> Item -> IO ()
downloadEach tokens path item = executeSync tokens $ Download item (localPath path item)

hidden :: FilePath -> Bool
hidden ('.':_) = True
hidden _ = False

localPath :: FilePath -> Item -> FilePath
localPath p i = p </> T.unpack (itemTitle i)
