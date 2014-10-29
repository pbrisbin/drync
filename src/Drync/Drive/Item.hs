{-# LANGUAGE OverloadedStrings #-}
module Drync.Drive.Item
    ( FileId
    , ChildList(..)
    , Item(..)
    , root
    ) where

import Control.Applicative
import Control.Monad (mzero)
import Data.Aeson
import Data.Text (Text)
import Data.Time (UTCTime)

type FileId = Text

newtype ChildList = ChildList [FileId]

instance FromJSON ChildList where
    parseJSON (Object o) = ChildList
        <$> (mapM (.: "id") =<< o .: "items")

    parseJSON _ = mzero

data Item = Item
    { itemId :: FileId
    , itemTitle :: Text
    , itemModified :: UTCTime
    , itemDownloadUrl :: Maybe Text
    }
    deriving (Eq, Show)

instance FromJSON Item where
    parseJSON (Object o) = Item
        <$> o .: "id"
        <*> o .: "title"
        <*> o .: "modifiedDate"
        <*> o .:? "downloadUrl"

    parseJSON _ = mzero

root :: FileId
root = "root"
