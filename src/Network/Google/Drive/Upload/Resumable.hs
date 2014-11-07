module Network.Google.Drive.Upload.Resumable
    ( postUpload
    , putUpload
    ) where

import Control.Concurrent (threadDelay)
import Control.Monad (void)
import Data.Aeson
import Data.ByteString (ByteString)
import Data.Conduit
import Data.Conduit.Binary (sourceFile, sourceFileRange)
import Data.Conduit.Progress
import Data.Conduit.Throttle
import Data.List (stripPrefix)
import Data.Monoid ((<>))
import Network.HTTP.Conduit
import Network.HTTP.Types
    ( Method
    , Status
    , hContentLength
    , hContentType
    , hLocation
    , hRange
    , mkStatus
    , statusIsServerError
    )
import System.Directory (getTemporaryDirectory, removeFile)
import System.FilePath ((</>), (<.>), isPathSeparator)
import System.IO
import System.Random (randomRIO)

import qualified Control.Exception as E
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C8

import Network.Google.Api
import Network.Google.Drive.File

baseUrl :: URL
baseUrl = "https://www.googleapis.com/upload/drive/v2"

postUpload :: ToJSON a => Path -> a -> FilePath -> Api File
postUpload = resumableUpload "POST"

putUpload :: ToJSON a => Path -> a -> FilePath -> Api File
putUpload = resumableUpload "PUT"

resumableUpload :: ToJSON a => Method -> Path -> a -> FilePath -> Api File
resumableUpload method path body filePath = do
    msessionUrl <- liftIO $ cachedSessionUrl filePath

    case msessionUrl of
        Nothing -> do
            cleaningUpCacheFile filePath $ do
                sessionUrl <- initiateUpload method path body
                liftIO $ cacheSessionUrl filePath sessionUrl
                beginUpload sessionUrl filePath

        Just sessionUrl -> do
            cleaningUpCacheFile filePath $ retryWithBackoff 1 $ do
                range <- getUploadedBytes sessionUrl filePath
                resumeUpload sessionUrl range filePath

initiateUpload :: ToJSON a => Method -> Path -> a -> Api URL
initiateUpload method path body = do
    response <- requestLbs (baseUrl <> path) $
        setMethod method .
        setQueryString uploadQuery .
        setBody (encode body) .
        addHeader (hContentType, "application/json")

    case lookup hLocation $ responseHeaders response of
        Just url -> return $ C8.unpack url
        Nothing -> throwApiError "Resumable upload Location header missing"

  where
    uploadQuery :: Params
    uploadQuery =
        [ ("uploadType", Just "resumable")
        , ("setModifiedDate", Just "true")
        ]

beginUpload :: URL -> FilePath -> Api File
beginUpload sessionUrl filePath = do
    fileLength <- liftIO $ withFile filePath ReadMode hFileSize

    let source = sourceFile filePath
        throttled = throttle B.length (800 * 1000) -- 800KB/s
        progress = reportProgress B.length (fromIntegral fileLength) 100
        modify = setMethod "PUT" .
            setBodySource
                (fromIntegral fileLength)
                (source $= throttled $= progress)

    requestJSON sessionUrl modify

getUploadedBytes :: URL -> FilePath -> Api Int
getUploadedBytes sessionUrl filePath = do
    fileLength <- liftIO $ withFile filePath ReadMode hFileSize

    response <- requestLbs sessionUrl $
        setMethod "PUT" .
        addHeader (hContentLength, "0") .
        addHeader ("Content-Range", "bytes */" <> C8.pack (show fileLength)) .
        allowStatus status308

    case lookup hRange $ responseHeaders response of
        Just range -> return $ rangeEnd range
        Nothing -> throwApiError "Resumable upload Range header missing"

  where
    rangeEnd :: ByteString -> Int
    rangeEnd b = case stripPrefix "0-" $ C8.unpack b of
        Just bs -> read bs
        Nothing -> 0

resumeUpload :: URL -> Int -> FilePath -> Api File
resumeUpload sessionUrl completed filePath = do
    fileLength <- liftIO $ withFile filePath ReadMode hFileSize

    let range = nextRange completed fileLength
        offset = fromIntegral $ completed + 1
        source = sourceFileRange filePath (Just offset) Nothing
        throttled = throttle B.length (800 * 1000) -- 800KB/s
        progress = reportProgress B.length (fromIntegral fileLength) 100

        modify =
            setMethod "PUT" .
            addHeader ("Content-Range", range) .
            setBodySource
                (fromIntegral fileLength)
                (source $= throttled $= progress)

    requestJSON sessionUrl modify

  where
    -- Content-Range: bytes 43-1999999/2000000
    nextRange c t = C8.pack $
        "bytes " <> show (c + 1) <> "-" <> show (t - 1) <> "/" <> show t

cachedSessionUrl :: FilePath -> IO (Maybe URL)
cachedSessionUrl filePath = do
    fp <- cacheFile filePath
    result <- try $ readFile fp

    return $ case result of
        Right url -> Just url
        _ -> Nothing

cacheSessionUrl :: FilePath -> URL -> IO ()
cacheSessionUrl filePath url = do
    fp <- cacheFile filePath

    void $ try $ writeFile fp url

cleaningUpCacheFile :: FilePath -> Api a -> Api a
cleaningUpCacheFile filePath action = do
    result <- action
    liftIO $ removeFile =<< cacheFile filePath
    return result

cacheFile :: FilePath -> IO FilePath
cacheFile filePath = do
    tmp <- getTemporaryDirectory
    return $ tmp </> "drive-upload" <> sanitize filePath <.> "session"
  where
    sanitize = map (replacePathSeparator '-')
    replacePathSeparator c x
        | isPathSeparator x = c
        | otherwise = x

retryWithBackoff :: Int -> Api a -> Api a
retryWithBackoff seconds f = f `catchError` \e ->
    if seconds < 16 && retryable e
        then delay >> retryWithBackoff (seconds * 2) f
        else throwError e

  where
    retryable :: ApiError -> Bool
    retryable (HttpError (StatusCodeException s _ _)) = statusIsServerError s
    retryable _ = False

    delay :: Api ()
    delay = liftIO $ do
        ms <- randomRIO (0, 999)
        threadDelay $ (seconds * 1000 + ms) * 1000

status308 :: Status
status308 = mkStatus 308 "Resume Incomplete"

try :: IO a -> IO (Either E.IOException a)
try = E.try
