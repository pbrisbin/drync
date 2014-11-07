module Network.Google.Drive.Upload.Resumable
    ( postUploadResumable
    , putUploadResumable
    ) where

import Control.Applicative ((<$>),(<*>))
import Control.Monad (void)
import Data.Aeson
import Data.ByteString (ByteString)
import Data.List (stripPrefix)
import Data.Maybe (fromMaybe)
import Data.Monoid ((<>))
import Network.HTTP.Conduit
import Network.HTTP.Types (Method, hLocation, hRange)
import System.Directory (getTemporaryDirectory)
import System.FilePath ((</>), (<.>), isPathSeparator)
import System.IO

import qualified Control.Exception as E
import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString.Lazy as BL

import Network.Google.Api

baseUrl :: URL
baseUrl = "https://www.googleapis.com/upload/drive/v2"

postUploadResumable :: (ToJSON a, FromJSON b) => Path -> a -> FilePath -> Api b
postUploadResumable = resumableUpload "POST"

putUploadResumable :: (ToJSON a, FromJSON b) => Path -> a -> FilePath -> Api b
putUploadResumable = resumableUpload "PUT"

resumableUpload :: (ToJSON a, FromJSON b)
                => Method
                -> Path
                -> a
                -> FilePath
                -> Api b
resumableUpload method path body filePath = do
    msessionUrl <- liftIO $ cachedSessionUrl filePath

    case msessionUrl of
        Nothing -> do
            sessionUrl <- beginUpload method path body
            liftIO $ cacheSessionUrl filePath sessionUrl

            resumeUpload sessionUrl Nothing filePath

        Just sessionUrl -> do
            range <- getUploadedBytes sessionUrl

            resumeUpload sessionUrl range filePath

getUploadedBytes :: URL -> Api (Maybe Int)
getUploadedBytes sessionUrl = do
    response <- requestLbs sessionUrl $ return . setMethod "PUT"

    return $ fmap rangeEnd $ lookup hRange $ responseHeaders response

  where
    rangeEnd :: ByteString -> Int
    rangeEnd b = case stripPrefix "0-" $ C8.unpack b of
        Just bs -> read bs
        Nothing -> 0

beginUpload :: ToJSON a => Method -> Path -> a -> Api URL
beginUpload method path body = do
    response <- requestLbs (baseUrl <> path) $ return .
        setMethod method .
        setQueryString uploadQuery .
        setBody (encode body)

    case lookup hLocation $ responseHeaders response of
        Just url -> return $ C8.unpack url
        Nothing -> throwApiError "Resumable upload Location header missing"

  where
    uploadQuery :: Params
    uploadQuery =
        [ ("uploadType", Just "resumable")
        , ("setModifiedDate", Just "1")
        ]

resumeUpload :: FromJSON a => URL -> Maybe Int -> FilePath -> Api a
resumeUpload sessionUrl mlen filePath = do
    let completed = fromMaybe 0 mlen

    (fileContents, fileLength) <-
        liftIO $ withFile filePath ReadMode $ \h -> do
            hSeek h AbsoluteSeek $ fromIntegral $ completed + 1

            (,) <$> BL.hGetContents h <*> hFileSize h

    let range = nextRange completed fileLength

    requestJSON sessionUrl $ return .
        setMethod "PUT" .
        setBody fileContents .
        addHeader ("Content-Range", range)

  where
    -- Content-Range: bytes 43-1999999/2000000
    nextRange c t = C8.pack $
        "bytes " <> show (c + 1) <> "-" <> show (t - 1) <> "/" <> show t

cachedSessionUrl :: FilePath -> IO (Maybe URL)
cachedSessionUrl filePath = do
    fp <- cacheFile filePath
    result <- fmap (fmap reads) $ try $ readFile fp

    return $ case result of
        Right ((url,_):_) -> Just url
        _ -> Nothing

cacheSessionUrl :: FilePath -> URL -> IO ()
cacheSessionUrl filePath url = do
    fp <- cacheFile filePath

    void $ try $ writeFile fp url

cacheFile :: FilePath -> IO FilePath
cacheFile filePath = do
    tmp <- getTemporaryDirectory
    return $ tmp </> "drive-upload" <> sanitize filePath <.> "session"
  where
    sanitize = map (replacePathSeparator '-')
    replacePathSeparator c x
        | isPathSeparator c = x
        | otherwise = c

try :: IO a -> IO (Either E.IOException a)
try = E.try
