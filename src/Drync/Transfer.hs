module Drync.Transfer
    ( upload
    , download
    ) where

import Drync.Options
import Drync.System

import Control.Monad (void)
import Control.Monad.IO.Class (MonadIO)
import Data.ByteString (ByteString)
import Data.Conduit
import Data.Conduit.Throttle
import Network.Google.Drive
import System.Console.AsciiProgress
    ( ProgressBar
    , def
    , complete
    , newProgressBar
    , pgTotal
    , pgOnCompletion
    , tickN
    )

import qualified Data.ByteString as B
import qualified Data.Foldable as F
import qualified Data.Text as T

upload :: Options -> FilePath -> File -> Api ()
upload options fp file = do
    size <- liftIO $ getFileSize fp

    let msize = Just size

    liftIO $ message options fp

    void $ updateFileWithContent (fileId file) (fileData file) size $ \c ->
        uploadSourceFile fp c $= transferConduit options msize

download :: Options -> File -> FilePath -> Api ()
download options file fp = do
    let fd = fileData file

    F.forM_ (fileDownloadUrl fd) $ \url -> do
        liftIO $ message options $ show file

        getSource (T.unpack url) [] $
            ($$+- transferConduit options (fileSize fd) =$ sinkFile fp)

        liftIO $ setModificationTime fp $ fileModified fd

transferConduit :: MonadIO m
                => Options
                -> Maybe Int
                -> Conduit ByteString m ByteString
transferConduit options msize =
    throttled (oThrottle options) =$= withProgress (oSilent options) msize

  where
    pass = await >>= maybe (return ()) (\v -> yield v >> pass)

    throttled 0 = pass
    throttled n = throttle B.length (n * 1000)

    withProgress False (Just size) = reportProgress size
    withProgress _ _ = pass

reportProgress :: MonadIO m => Int -> Conduit ByteString m ByteString
reportProgress size = do
    pg <- liftIO $ newProgressBar def
        { pgTotal = size
        , pgOnCompletion = putStrLn "Done."
        }

    updateProgress pg

updateProgress :: MonadIO m => ProgressBar -> Conduit ByteString m ByteString
updateProgress pg = await >>= maybe
    (liftIO $ complete pg >> return ())
    (\chunk -> do
        liftIO $ tickN pg $ B.length chunk
        yield chunk
        updateProgress pg)
