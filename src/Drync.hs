module Drync where

import Control.Applicative ((<$>), (<*>))
import Control.Monad (filterM, when, unless, void)
import Control.Monad.Reader (ReaderT(..), asks, lift)
import Control.Monad.IO.Class (MonadIO)
import Data.ByteString (ByteString)
import Data.Conduit
import Data.Conduit.Progress (reportProgress)
import Data.Conduit.Throttle (throttle)
import Data.Monoid ((<>))
import Data.List (partition)
import Data.Time (UTCTime, diffUTCTime, getCurrentTime)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
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
import System.FilePath.Glob (match)
import System.IO
    ( IOMode(..)
    , hFileSize
    , hPutStrLn
    , stderr
    , withFile
    )
import System.Posix (EpochTime, setFileTimes)

import qualified Data.Text as T
import qualified Data.ByteString as B

import Drync.Options

type Sync = ReaderT Options Api

runSync :: Options -> Sync a -> Api a
runSync = flip runReaderT

sync :: FilePath -> File -> Sync ()
sync filePath file = do
    info $ "SYNC " <> filePath <> " <--> " <> show file

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
    files <- lift $ listFiles $ ParentEq (fileId file) `And` Untrashed
    paths <- liftIO $ getVisibleDirectoryContents filePath

    debug "Local contents:"
    mapM_ (debug . ("  " <>)) paths

    debug "Remote contents:"
    mapM_ (debug . ("  " <>) . show) files

    -- probably inefficient but hopefuly these are small enough lists
    let (both, remote) = partition ((`elem` paths) . localPath) files
        local = filter (`notElem` map localPath both) paths

    forIncluded id local $ \fp -> create (filePath </> fp) (fileId file)
    forIncluded localPath remote $ \f -> download f $ filePath </> localPath f
    forIncluded localPath both $ \f -> sync (filePath </> localPath f) f

create :: FilePath -> FileId -> Sync ()
create filePath parent = do
    isDirectory <- liftIO $ doesDirectoryExist filePath
    if isDirectory
        then createDirectory filePath parent
        else upload filePath =<< lift (newFile parent filePath)

createDirectory :: FilePath -> FileId -> Sync ()
createDirectory filePath parent = do
    let name = takeFileName filePath

    info $ "CREATE FOLDER " <> name
    paths <- liftIO $ getVisibleDirectoryContents filePath
    folder <- lift $ createFolder parent $ T.pack name
    forIncluded id paths $ \fp -> create (filePath </> fp) $ fileId folder

upload :: FilePath -> File -> Sync ()
upload filePath file = do
    t <- asks oThrottle
    p <- asks oProgress

    info $ "UPLOAD " <> filePath <> " --> " <> show file
    size <- liftIO $ withFile filePath ReadMode hFileSize
    void $ lift $ uploadFile file (fromIntegral size) $ \c ->
        uploadSourceFile filePath c
        $= throttled t
        $= withProgress p (Just $ fromIntegral size)

download :: File -> FilePath -> Sync ()
download file filePath =
    case fileDownloadUrl $ fileData file of
        Nothing -> do
            when (isFolder file) $
                downloadDirectory file filePath
        Just url -> do
            t <- asks oThrottle
            p <- asks oProgress

            info $ "DOWNLOAD " <> show file <> " --> " <> filePath
            lift $ getSource (T.unpack url) [] $ \source ->
                source $$+-
                    throttled t
                    =$ withProgress p (fileSize $ fileData file)
                    =$ sinkFile filePath

            -- Equalize timestamps
            liftIO $ setModificationTime filePath $ fileModified $ fileData file

downloadDirectory :: File -> FilePath -> Sync ()
downloadDirectory file filePath = do
    files <- lift $ listFiles $ ParentEq (fileId file)

    debug $ "Remote contents:"
    mapM_ (debug . ("  " <>) . show) files

    liftIO $ createDirectoryIfMissing True filePath
    forIncluded localPath files $ \f ->
        download f $ filePath </> (T.unpack $ fileTitle $ fileData f)

forIncluded :: (a -> String) -> [a] -> (a -> Sync b) -> Sync ()
forIncluded f xs k = mapM_ k =<< filterM (isIncluded . f) xs

isIncluded :: String -> Sync Bool
isIncluded name = do
    excludes <- asks oExcludes
    return $ not $ any (`match` name) excludes

getVisibleDirectoryContents :: FilePath -> IO [FilePath]
getVisibleDirectoryContents path = filter visible <$> getDirectoryContents path

  where
    visible :: FilePath -> Bool
    visible ('.':_) = False
    visible _ = True

info :: String -> Sync ()
info msg = do
    s <- asks oSilent
    unless s $ liftIO $ putStrLn msg

debug :: String -> Sync ()
debug msg = do
    d <- asks oDebug
    when d $ liftIO $ hPutStrLn stderr $ "[DEBUG] " <> msg

throw :: String -> Sync ()
throw = lift . throwApiError

throttled :: MonadIO m => Int -> Conduit ByteString m ByteString
throttled 0 = pass
throttled n = throttle B.length (n * 1000)

withProgress :: MonadIO m => Int -> Maybe Int -> Conduit ByteString m ByteString
withProgress 0 _ = pass
withProgress n (Just size) = reportProgress B.length size n
withProgress _ _ = pass

pass :: Monad m => Conduit o m o
pass = await >>= maybe (return ()) (\v -> yield v >> pass)

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
