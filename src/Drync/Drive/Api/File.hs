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

import Control.Applicative ((<$>), (<*>))
import Control.Monad (mzero)
import Data.Aeson (FromJSON(..), Value(..), (.=), (.:), (.:?), object)
import Data.Maybe (listToMaybe)
import Data.Monoid ((<>))
import Data.Text (Text)
import Data.Time (UTCTime)

import qualified Data.Text as T

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

createFolder :: FileId -> Text -> Api (Maybe Item)
createFolder parentId name = do
    logApi $ "CREATE FOLDER " <> T.unpack parentId <> "/" <> T.unpack name

    postApi "/files" $ object
        [ "title" .= name
        , "parents" .= [object ["id" .= parentId]]
        , "mimeType" .= folderType
        ]

  where
    folderType :: Text
    folderType = "application/vnd.google-apps.folder"

-- TODO
createFile :: FilePath -> Item -> Api FileId
createFile path parent = do
    logApi $ "CREATE " <> path <> " --> " <> show parent

    return "new"

-- TODO
updateFile :: FilePath -> Item -> Api ()
updateFile path item =
    logApi $ "UPDATE " <> path <> " --> " <> show item

downloadFile :: Item -> FilePath -> Api ()
downloadFile item path = case itemDownloadUrl item of
    Nothing -> logApi $ show item <> " had no Download URL"
    Just url -> do
        logApi $ "DOWNLOAD " <> show item <> " --> " <> path
        authenticatedDownload (T.unpack url) path
