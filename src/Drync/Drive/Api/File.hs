module Drync.Drive.Api.File
    ( FileId
    , Items(..)
    , Item(..) -- TODO: rename to File?
    , getFile
    , createFolder
    , createFile
    , updateFile
    , downloadFile
    ) where

import Control.Applicative
import Control.Monad (mzero)
import Data.Aeson
import Data.Maybe (listToMaybe)
import Data.Monoid ((<>))
import Data.Text (Text)
import Data.Time (UTCTime, getCurrentTime)

import qualified Data.Text as T
import qualified Data.Text.IO as T

import Drync.Drive.Api.HTTP

type FileId = Text

newtype Items = Items [Item]

instance FromJSON Items where
    parseJSON (Object o) = Items
        <$> (mapM parseJSON =<< o .: "items")

    parseJSON _ = mzero

data Item = Item
    { itemId :: FileId
    , itemTitle :: Text
    , itemModified :: UTCTime
    , itemParent :: Maybe FileId
    , itemTrashed :: Bool
    , itemDownloadUrl :: Maybe Text
    }

instance Eq Item where
    a == b = itemId a == itemId b

instance Show Item where
    show Item{..} = T.unpack $ itemTitle <> " (" <> itemId <> ")"

instance FromJSON Item where
    parseJSON (Object o) = Item
        <$> o .: "id"
        <*> o .: "title"
        <*> o .: "modifiedDate"
        <*> (listToMaybe <$> (mapM (.: "id") =<< o .: "parents"))
        <*> ((.: "trashed") =<< o .: "labels")
        <*> o .:? "downloadUrl"

    parseJSON _ = mzero

getFile :: FileId -> Api (Maybe Item)
getFile fileId = simpleApi $ "/files/" <> T.unpack fileId

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
