module Drync where

import Control.Applicative ((<$>), (<*>))
import Control.Monad (forM_, when, void)
import Data.Conduit
import Data.Conduit.Binary (sinkFile, sourceFileRange)
import Data.Monoid ((<>))
import Data.Time (UTCTime, diffUTCTime)
import Network.Google.Api
import Network.Google.Drive.File
import Network.Google.Drive.Upload
import System.Directory
    ( createDirectoryIfMissing
    , doesDirectoryExist
    , doesFileExist
    , getDirectoryContents
    , getModificationTime
    )
import System.FilePath ((</>), takeFileName)
import System.IO (IOMode(..), hFileSize, withFile)

import qualified Data.Text as T

sync :: FilePath -> File -> Api ()
sync filePath file = do
    isFileDirectory <- liftIO $ (,)
        <$> doesFileExist filePath
        <*> doesDirectoryExist filePath

    case isFileDirectory of
        (True, _) -> syncFile filePath file
        (_, True) -> syncDirectory filePath file
        _ -> throwApiError $ "File not found: " <> filePath

syncFile :: FilePath -> File -> Api ()
syncFile filePath file = do
    modified <- liftIO $ getModificationTime filePath
    let rmodified = fileModified $ fileData file
    when (different modified rmodified) $
        if modified > rmodified
            then update file filePath
            else download file filePath

  where
    -- Downloading or uploading results in a small difference in modification
    -- times. We should ignore such differences so as to not continually
    -- re-sync files back and forth.
    different :: UTCTime -> UTCTime -> Bool
    different x = (> 30) . abs . diffUTCTime x

syncDirectory :: FilePath -> File -> Api ()
syncDirectory = undefined --filePath file = do
    -- paths <- liftIO $ getVisibleDirectoryContents filePath
    -- files <- listFiles $ ParentEq (fileId file)

    -- probably inefficient but hopefuly these are small enough lists
    -- let both = filter ((`elem` paths) . T.unpack . fileTitle) files
    --     local = filter ((`notElem` map fileTitle both) . T.pack) paths
    --     remote = filter (`notElem` both) files

update :: File -> FilePath -> Api ()
update file filePath = do
    size <- liftIO $ withFile filePath ReadMode hFileSize
    void $ uploadFile file (fromIntegral size) $ \c ->
        sourceFileRange filePath (Just $ fromIntegral $ c + 1) Nothing

upload :: FilePath -> FileId -> Api ()
upload filePath parent = do
    isDirectory <- liftIO $ doesDirectoryExist filePath
    if isDirectory
        then uploadDirectory filePath parent
        else do
            size <- liftIO $ withFile filePath ReadMode hFileSize
            fdata <- newFile parent filePath
            void $ uploadNewFile fdata (fromIntegral size) $ \c ->
                sourceFileRange filePath (Just $ fromIntegral $ c + 1) Nothing

uploadDirectory :: FilePath -> FileId -> Api ()
uploadDirectory filePath parent = do
    paths <- liftIO $ getVisibleDirectoryContents filePath
    folder <- insertFile =<<
        newFolder parent (T.pack $ takeFileName filePath)
    forM_ paths $ \path -> upload (filePath </> path) $ fileId folder

download :: File -> FilePath -> Api ()
download file filePath =
    case fileDownloadUrl $ fileData file of
        Nothing -> downloadDirectory file filePath
        Just url -> getSource (T.unpack url) [] $ ($$+- sinkFile filePath)

downloadDirectory :: File -> FilePath -> Api ()
downloadDirectory file filePath = do
    files <- listFiles $ ParentEq (fileId file)
    liftIO $ createDirectoryIfMissing True filePath
    forM_ files $ \f ->
        download f $ filePath </> (T.unpack $ fileTitle $ fileData f)

getVisibleDirectoryContents :: FilePath -> IO [FilePath]
getVisibleDirectoryContents path = filter visible <$> getDirectoryContents path

  where
    visible :: FilePath -> Bool
    visible ('.':_) = False
    visible _ = True

info :: String -> Api ()
info = liftIO . putStrLn

--
-- TODO: Progress/Throttling
--
-- -- | Convert the given sink to one with throttling and/or progress reporting
-- --   depending on the current @ApiOptions@
-- downloadSink :: MonadIO m
--              => Maybe Int
--              -> Sink ByteString m r
--              -> Api (Sink ByteString m r)
-- downloadSink msize sink = do
--     options <- fmap apiOptions ask

--     return $ case (msize, options) of
--         (Just s, ApiOptions _ (Just l) (Just p) _) ->
--             throttled l =$ progress p s =$ sink
--         (Just s, ApiOptions _ _ (Just p) _) -> progress p s =$ sink
--         (_, ApiOptions _ (Just l) _ _) -> throttled l =$ sink
--         _ -> sink

-- -- | Convert the given source to one with throttling and/or progress reporting
-- --   depending on the current @ApiOptions@
-- uploadSource :: MonadIO m
--              => Maybe Int
--              -> Source m ByteString
--              -> Api (Source m ByteString)
-- uploadSource msize source = do
--     options <- fmap apiOptions ask

--     return $ case (msize, options) of
--         (Just s, ApiOptions _ (Just l) (Just p) _) ->
--             source $= throttled l $= progress p s
--         (Just s, ApiOptions _ _ (Just p) _) -> source $= progress p s
--         (_, ApiOptions _ (Just l) _ _) -> source $= throttled l
--         _ -> source

-- throttled :: MonadIO m => Int -> Conduit ByteString m ByteString
-- throttled = throttle B.length

-- progress :: MonadIO m => Int -> Int -> Conduit ByteString m ByteString
-- progress each size = reportProgress B.length size each