{-# LANGUAGE OverloadedStrings #-}
-- |
--
-- TODO: Exception handling
--
module Drync.Drive.Api
    ( getFile
    , getFiles
    , getFilesByParent
    , getFolder
    ) where

import Control.Monad (when)
import Data.Aeson (decode)
import Data.Maybe (listToMaybe)
import Data.Monoid ((<>))
import Data.Text (Text)
import Network.HTTP.Base (urlEncode)
import Network.HTTP.Conduit (simpleHttp)

import qualified Data.Text as T

import Drync.Token
import Drync.Drive.Item

baseUrl :: String
baseUrl = "https://www.googleapis.com/drive/v2"

-- | Find a file by Id
getFile :: OAuth2Tokens -> FileId -> IO (Maybe Item)
getFile tokens fileId = fmap decode $ simpleHttp $ baseUrl <>
    "/files/" <> T.unpack fileId <> "?access_token=" <> accessToken tokens

-- | Find files matching the given query
getFiles :: OAuth2Tokens -> Text -> IO [Item]
getFiles tokens query = do
    mlist <- fmap decode $ simpleHttp $ baseUrl <>
        "/files" <> "?access_token=" <> accessToken tokens <>
        "&q=" <> urlEncode (T.unpack query) <> "&maxResults=1000"

    return $ case mlist of
        Just (Items items) -> unTrashed items
        Nothing -> []

-- | Return all child Items of the given folder
getFilesByParent :: OAuth2Tokens -> FileId -> IO [Item]
getFilesByParent tokens fileId = getFiles tokens $ quote fileId <> " in parents"

-- TODO: handle abiguities since this just searches by name
getFolder :: OAuth2Tokens -> Text -> IO (Maybe Item)
getFolder tokens path = do
    folders <- getFiles tokens $ "title = " <> quote path

    when (length folders > 1) $
        putStrLn "warning: folder name returned multiple results"

    return $ listToMaybe folders

quote :: Text -> Text
quote = ("'" <>) . (<> "'")
