{-# LANGUAGE OverloadedStrings #-}
module Drync.Drive.Item
    ( FileId
    , Items(..)
    , Item(..)
    , unTrashed
    , root
    ) where

import Control.Applicative
import Control.Monad (mzero)
import Data.Aeson
import Data.Text (Text)
import Data.Time (UTCTime)

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
    , itemTrashed :: Bool
    , itemDownloadUrl :: Maybe Text
    }
    deriving (Eq, Show)

instance FromJSON Item where
    parseJSON (Object o) = Item
        <$> o .: "id"
        <*> o .: "title"
        <*> o .: "modifiedDate"
        <*> ((.: "trashed") =<< o .: "labels")
        <*> o .:? "downloadUrl"

    parseJSON _ = mzero

unTrashed :: [Item] -> [Item]
unTrashed = filter (not . itemTrashed)

root :: FileId
root = "root"
