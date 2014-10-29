{-# LANGUAGE OverloadedStrings #-}
module Drync.Drive.FileList where

import Control.Applicative
import Control.Monad (mzero)
import Data.Aeson
import Data.Text (Text)
import Data.Time (UTCTime)
import Data.Maybe (listToMaybe)

type ItemId = Text

newtype FileList = FileList [Item] deriving Show

instance FromJSON FileList where
    parseJSON (Object o) = FileList
        <$> (mapM parseJSON =<< o .: "items")

    parseJSON _ = mzero

data Item = Item
    { itemId :: ItemId
    , itemTitle :: Text
    , itemModified :: UTCTime
    , itemParents :: Maybe ItemId
    }
    deriving (Eq, Show)

instance FromJSON Item where
    parseJSON (Object o) = Item
        <$> o .: "id"
        <*> o .: "title"
        <*> o .: "modifiedDate"
        <*> (listToMaybe <$> (mapM (.: "id") =<< o .: "parents"))

    parseJSON _ = mzero
