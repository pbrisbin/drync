module Drync.Drive.Api
    ( Query(..)
    , getFile
    , getFiles
    , createFolder
    , createFile
    , updateFile
    , downloadFile
    ) where

import Data.Aeson --(decode, encode)
import Data.Monoid ((<>))
import Data.Text (Text)
import Data.Time (getCurrentTime)
import Network.HTTP.Base (urlEncode)
import Network.HTTP.Conduit --(Request(..), RequestBody(..), parseUrl, simpleHttp)
--import Network.HTTP.Types (Headers, hAuthorization, hContentType)

import qualified Data.Text as T
import qualified Data.Text.IO as T

import Drync.Token
import Drync.Drive.Item

data Query
    = TitleEq Text
    | ParentEq FileId
    | Query `And` Query
    | Query `Or` Query

toParam :: Query -> String
toParam = urlEncode . T.unpack . toParam'

  where
    toParam' (TitleEq title) = "title = " <> quote title
    toParam' (ParentEq fileId) = quote fileId <> " in parents"
    toParam' (p `And` q) = "(" <> toParam' p <> ") and (" <> toParam' q <> ")"
    toParam' (p `Or` q) = "(" <> toParam' p <> ") or (" <> toParam' q <> ")"

    quote = ("'" <>) . (<> "'")

baseUrl :: String
baseUrl = "https://www.googleapis.com/drive/v2"

getFile :: OAuth2Tokens -> FileId -> IO (Maybe Item)
getFile tokens fileId = fmap decode $ simpleHttp $ baseUrl <>
    "/files/" <> T.unpack fileId <> "?access_token=" <> accessToken tokens

getFiles :: OAuth2Tokens -> Query -> IO [Item]
getFiles tokens query = do
    mlist <- fmap decode $ simpleHttp $ baseUrl <>
        "/files" <> "?access_token=" <> accessToken tokens <>
        "&q=" <> toParam query <> "&maxResults=1000"

    return $ case mlist of
        Just (Items items) -> unTrashed items
        Nothing -> []

createFolder :: OAuth2Tokens -> FileId -> Text -> IO Item
createFolder _ parentId name = do
    T.putStrLn $ "CREATE FOLDER " <> parentId <> "/" <> name

    now <- getCurrentTime

    return Item
            { itemId = "new"
            , itemTitle = name
            , itemModified = now
            , itemParent = Just $ parentId
            , itemTrashed = False
            , itemDownloadUrl = Nothing
            }

createFile :: OAuth2Tokens -> FilePath -> Item -> IO FileId
createFile _ path item = do
    putStrLn $ "CREATE " <> path <> " --> " <> show item

    return "new"

updateFile :: OAuth2Tokens -> FilePath -> Item -> IO ()
updateFile _ path item =
    putStrLn $ "UPDATE " <> path <> " --> " <> show item

downloadFile :: OAuth2Tokens -> Item -> FilePath -> IO ()
downloadFile _ item path =
    putStrLn $ "DOWNLOAD " <> show item <> " --> " <> path

-- createFolder :: OAuth2Tokens -> FileId -> Text -> IO (Maybe Item)
-- createFolder tokens parentId folder = do
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
