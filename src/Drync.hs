module Drync where

import Control.Applicative
import Control.Monad.Reader hiding (local)
import Data.ByteString (ByteString)
import Data.Conduit
import Data.Conduit.Binary hiding (mapM_)
import Data.Conduit.Progress
import Data.Conduit.Throttle
import Data.Monoid
import Data.List
import Data.Time
import Network.Google.Api
import Network.Google.Drive.File
import Network.Google.Drive.Upload
import System.Directory
import System.FilePath
import System.IO

import qualified Data.Text as T
import qualified Data.ByteString as B

import Drync.Options

type Sync = ReaderT Options Api

runSync :: Options -> Sync a -> Api a
runSync = flip runReaderT

sync :: FilePath -> File -> Sync ()
sync filePath file = do
    info "SYNC" $ filePath <> " <--> " <> show file

    isFileDirectory <- liftIO $ (,)
        <$> doesFileExist filePath
        <*> doesDirectoryExist filePath

    case isFileDirectory of
        (True, _) -> syncFile filePath file
        (_, True) -> syncDirectory filePath file
        _ -> throw $ "File not found: " <> filePath

syncFile :: FilePath -> File -> Sync ()
syncFile filePath file = do
    modified <- liftIO $ getModificationTime filePath
    let rmodified = fileModified $ fileData file

    info "SYNC" $ filePath <> " <--> " <> show file
    debug $ "Local modified :" <> show modified
    debug $ "Remote modified:" <> show rmodified

    when (different modified rmodified) $
        if modified > rmodified
            then upload filePath file
            else download file filePath

  where
    -- Downloading or uploading results in a small difference in modification
    -- times. We should ignore such differences so as to not continually
    -- re-sync files back and forth.
    different :: UTCTime -> UTCTime -> Bool
    different x = (> 30) . abs . diffUTCTime x

syncDirectory :: FilePath -> File -> Sync ()
syncDirectory filePath file = do
    files <- lift $ listFiles $ ParentEq (fileId file)
    paths <- liftIO $ getVisibleDirectoryContents filePath

    debug "Local contents:"
    mapM_ (debug . ("  " <>)) paths

    debug "Remote contents:"
    mapM_ (debug . ("  " <>) . show) files

    -- probably inefficient but hopefuly these are small enough lists
    let (both, remote) = partition ((`elem` paths) . localPath) files
        local = filter (`notElem` map localPath both) paths

    forIncludedL_ local $ \fp -> create (filePath </> fp) (fileId file)
    forIncludedR_ remote $ \f -> download f $ filePath </> localPath f
    forIncludedR_ both $ \f -> sync (filePath </> localPath f) file

create :: FilePath -> FileId -> Sync ()
create filePath parent = do
    let title = T.pack $ takeFileName filePath

    isDirectory <- liftIO $ doesDirectoryExist filePath

    if isDirectory
        then do
            fps <- liftIO $ getVisibleDirectoryContents filePath
            folder <- lift $ insertFile =<< newFolder parent title

            info "CREATE DIRECTORY" $ show folder
            forM_ fps $ \fp -> create (filePath </> fp) $ fileId folder
        else do
            return ()
            f <- lift $ newFile parent filePath
            upload filePath f

upload :: FilePath -> File -> Sync ()
upload filePath file = do
    t <- asks oThrottle
    p <- asks oProgress

    info "UPLOAD" $ filePath <> " --> " <> show file
    size <- liftIO $ withFile filePath ReadMode hFileSize
    void $ lift $ uploadFile file (fromIntegral size) $ \c ->
        sourceFileRange filePath (Just $ fromIntegral $ c + 1) Nothing
        $= withProgress p (Just $ fromIntegral size)
        $= throttled t

download :: File -> FilePath -> Sync ()
download file filePath =
    case fileDownloadUrl $ fileData file of
        Nothing -> do
            debug $ "No download URL: " <> show file
            downloadDirectory file filePath
        Just url -> do
            t <- asks oThrottle
            p <- asks oProgress

            info "DOWNLOAD" $ show file <> " --> " <> filePath
            lift $ getSource (T.unpack url) [] $ \source ->
                source $$+-
                    throttled t
                    =$ withProgress p (fileSize $ fileData file)
                    =$ sinkFile filePath

downloadDirectory :: File -> FilePath -> Sync ()
downloadDirectory file filePath = do
    files <- lift $ listFiles $ ParentEq (fileId file)

    debug $ "Files to download:"
    mapM_ (debug . ("  " <>) . show) files

    liftIO $ createDirectoryIfMissing True filePath
    forM_ files $ \f ->
        download f $ filePath </> (T.unpack $ fileTitle $ fileData f)

forIncludedL_ :: [FilePath] -> (FilePath -> Sync a) -> Sync ()
forIncludedL_ fs k = mapM_ k =<< filterM isIncluded fs

forIncludedR_ :: [File] -> (File -> Sync a) -> Sync ()
forIncludedR_ fs k = mapM_ k =<<
    filterM (isIncluded . T.unpack . fileTitle . fileData) fs

isIncluded :: String -> Sync Bool
isIncluded name = do
    excludes <- asks oExcludes
    return $ not $ name `elem` excludes

getVisibleDirectoryContents :: FilePath -> IO [FilePath]
getVisibleDirectoryContents path = filter visible <$> getDirectoryContents path

  where
    visible :: FilePath -> Bool
    visible ('.':_) = False
    visible _ = True

debug :: String -> Sync ()
debug msg = do
    d <- asks oDebug
    when d $ liftIO $ hPutStrLn stderr $ "[DEBUG] " <> msg

info :: String -> String -> Sync ()
info act msg = liftIO $ putStrLn $ "--> " <> pad 15 act <> msg

throw :: String -> Sync ()
throw = lift . throwApiError

throttled :: MonadIO m => Int -> Conduit ByteString m ByteString
throttled 0 = pass
throttled n = throttle B.length n

withProgress :: MonadIO m => Int -> Maybe Int -> Conduit ByteString m ByteString
withProgress 0 _ = pass
withProgress n (Just size) = reportProgress B.length size n
withProgress _ _ = pass

pass :: Monad m => Conduit o m o
pass = maybe (return ()) yield =<< await
