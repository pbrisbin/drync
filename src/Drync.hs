module Drync where

import Control.Applicative
import Control.Monad.Reader
import Data.Conduit
import Data.Conduit.Binary
import Data.Monoid
import Data.Time
import Network.Google.Api
import Network.Google.Drive.File
import Network.Google.Drive.Upload
import System.Directory
import System.FilePath
import System.IO

import qualified Data.Text as T

import Drync.Options

type Sync = ReaderT Options Api

runSync :: Options -> Sync a -> Api a
runSync = flip runReaderT

sync :: FilePath -> File -> Sync ()
sync filePath file = do
    isFileDirectory <- liftIO $ (,)
        <$> doesFileExist filePath
        <*> doesDirectoryExist filePath

    case isFileDirectory of
        (True, _) -> syncFile filePath file
        (_, True) -> syncDirectory filePath file
        _ -> lift $ throwApiError $ "File not found: " <> filePath

syncFile :: FilePath -> File -> Sync ()
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

syncDirectory :: FilePath -> File -> Sync ()
syncDirectory = undefined --filePath file = do
    -- paths <- liftIO $ getVisibleDirectoryContents filePath
    -- files <- lift $ listFiles $ ParentEq (fileId file)

    -- probably inefficient but hopefuly these are small enough lists
    -- let both = filter ((`elem` paths) . T.unpack . fileTitle) files
    --     local = filter ((`notElem` map fileTitle both) . T.pack) paths
    --     remote = filter (`notElem` both) files

update :: File -> FilePath -> Sync ()
update file filePath = do
    size <- liftIO $ withFile filePath ReadMode hFileSize
    void $ lift $ uploadFile file (fromIntegral size) $ \c ->
        sourceFileRange filePath (Just $ fromIntegral $ c + 1) Nothing

upload :: FilePath -> FileId -> Sync ()
upload filePath parent = do
    isDirectory <- liftIO $ doesDirectoryExist filePath
    if isDirectory
        then uploadDirectory filePath parent
        else do
            size <- liftIO $ withFile filePath ReadMode hFileSize
            lift $ do
                fdata <- newFile parent filePath
                void $ uploadNewFile fdata (fromIntegral size) $ \c ->
                    sourceFileRange filePath (Just $ fromIntegral $ c + 1) Nothing

uploadDirectory :: FilePath -> FileId -> Sync ()
uploadDirectory filePath parent = do
    paths <- liftIO $ getVisibleDirectoryContents filePath
    folder <- lift $ insertFile =<<
        newFolder parent (T.pack $ takeFileName filePath)
    forM_ paths $ \path -> upload (filePath </> path) $ fileId folder

download :: File -> FilePath -> Sync ()
download file filePath =
    case fileDownloadUrl $ fileData file of
        Nothing -> downloadDirectory file filePath
        Just url -> lift $
            getSource (T.unpack url) [] $ ($$+- sinkFile filePath)

downloadDirectory :: File -> FilePath -> Sync ()
downloadDirectory file filePath = do
    files <- lift $ listFiles $ ParentEq (fileId file)
    liftIO $ createDirectoryIfMissing True filePath
    forM_ files $ \f ->
        download f $ filePath </> (T.unpack $ fileTitle $ fileData f)

getVisibleDirectoryContents :: FilePath -> IO [FilePath]
getVisibleDirectoryContents path = filter visible <$> getDirectoryContents path

  where
    visible :: FilePath -> Bool
    visible ('.':_) = False
    visible _ = True

info :: String -> Sync ()
info = liftIO . putStrLn

debug :: String -> Sync ()
debug msg = do
    d <- fmap oDebug ask

    when d $ liftIO $ hPutStrLn stderr msg

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
