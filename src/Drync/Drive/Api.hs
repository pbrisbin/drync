module Drync.Drive.Api
    ( Api
    , runApi
    , Query(..)
    , getFile
    , getFiles
    , createFolder
    , createFile
    , updateFile
    , downloadFile

    -- Re-exports
    , liftIO
    ) where

import Control.Monad.Reader
import Data.Aeson --(decode, encode)
import Data.ByteString (ByteString)
import Data.Monoid ((<>))
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import Data.Time (getCurrentTime)
import Network.HTTP.Conduit --(Request(..), RequestBody(..), parseUrl, simpleHttp)
--import Network.HTTP.Types (Headers, hAuthorization, hContentType)

import qualified Data.ByteString.Char8 as C8
import qualified Data.Text as T
import qualified Data.Text.IO as T

import Drync.Token
import Drync.Drive.Item

type Api a = ReaderT OAuth2Tokens IO a

runApi :: OAuth2Tokens -> Api a -> IO a
runApi tokens f = runReaderT f tokens

data Query
    = TitleEq Text
    | ParentEq FileId
    | Query `And` Query
    | Query `Or` Query

toParam :: Query -> ByteString
toParam (TitleEq title) = "title = " <> quote title
toParam (ParentEq fileId) = quote fileId <> " in parents"
toParam (p `And` q) = "(" <> toParam p <> ") and (" <> toParam q <> ")"
toParam (p `Or` q) = "(" <> toParam p <> ") or (" <> toParam q <> ")"

quote :: Text -> ByteString
quote = ("'" <>) . (<> "'") . encodeUtf8

type Path = String
type Params = [(ByteString, Maybe ByteString)]

baseUrl :: String
baseUrl = "https://www.googleapis.com/drive/v2"

simpleApi :: FromJSON a => Path -> Api (Maybe a)
simpleApi path = getApi path []

getApi :: FromJSON a => Path -> Params -> Api (Maybe a)
getApi path query = do
    request <- withToken query =<< liftIO (parseUrl $ baseUrl <> path)

    fmap (decode . responseBody) $ withManager $ httpLbs request

withToken :: Params -> Request -> Api Request
withToken query request = do
    tokens <- ask

    let token = C8.pack $ accessToken tokens
    let query' = ("access_token", Just token):query

    return $ setQueryString query' request

getFile :: FileId -> Api (Maybe Item)
getFile fileId = simpleApi $ "/files/" <> T.unpack fileId

getFiles :: Query -> Api [Item]
getFiles query = do
    let query' =
            [ ("q", Just $ toParam query)
            , ("maxResults", Just "1000")
            ]

    mlist <- getApi "/files" query'

    return $ case mlist of
        Just (Items items) -> unTrashed items
        Nothing -> []

createFolder :: FileId -> Text -> Api Item
createFolder parentId name = do
    liftIO $ T.putStrLn $ "CREATE FOLDER " <> parentId <> "/" <> name

    now <- liftIO getCurrentTime

    return Item
            { itemId = "new"
            , itemTitle = name
            , itemModified = now
            , itemParent = Just $ parentId
            , itemTrashed = False
            , itemDownloadUrl = Nothing
            }

createFile :: FilePath -> Item -> Api FileId
createFile path item = do
    liftIO $ putStrLn $ "CREATE " <> path <> " --> " <> show item

    return "new"

updateFile :: FilePath -> Item -> Api ()
updateFile path item =
    liftIO $ putStrLn $ "UPDATE " <> path <> " --> " <> show item

downloadFile :: Item -> FilePath -> Api ()
downloadFile item path =
    liftIO $ putStrLn $ "DOWNLOAD " <> show item <> " --> " <> path

-- createFolder :: FileId -> Text -> Api (Maybe Item)
-- createFolder parentId folder = do
--     request' <- parseUrl $ baseUrl <> "/files"

--     let
--         request = addHeaders headers $ request'
--             { method = "POST"
--             , requestBody = RequestBodyLBS $ encode body
--             }

--     return Nothing -- TODO

--   where
--     headers :: Headers
--     headers =
--             [ (hAuthorization, "Bearer " <> show (accessToken tokens))
--             , (hContentType, "application/json")
--             ]

--     body :: Value
--     body = object
--         [ "title" .= folder
--         , "parents" .= (object ["id" .= parentId])
--         , "mimeType" .= ("application/vnd.google-apps.folder" :: Text)
--         ]

--     addHeaders = undefined

-- POST https://www.googleapis.com/drive/v2/files
-- Authorization: Bearer {ACCESS_TOKEN}
-- Content-Type: application/json
-- ...
-- {
--       "title": "pets",
--         "parents": [{"id":"0ADK06pfg"}]
--           "mimeType": "application/vnd.google-apps.folder"
-- }
