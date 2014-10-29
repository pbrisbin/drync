-- |
--
-- TODO: Exception handling
--
module Drync.Drive.Api
    ( getFile
    , getChildren
    ) where

import Data.Aeson (decode)
import Data.Monoid ((<>))
import Network.HTTP.Conduit (simpleHttp)

import qualified Data.Text as T

import Drync.Token
import Drync.Drive.Item

baseUrl :: String
baseUrl = "https://www.googleapis.com/drive/v2"

getFile :: OAuth2Tokens -> FileId -> IO (Maybe Item)
getFile tokens fileId = fmap decode $ simpleHttp $ baseUrl <>
    "/files/" <> T.unpack fileId <> "?access_token=" <> accessToken tokens

-- | Does not support more than 1000 files per folder
getChildren :: OAuth2Tokens -> FileId -> IO [FileId]
getChildren tokens fileId = do
    mlist <- fmap decode $ simpleHttp $ baseUrl <>
        "/files/" <> T.unpack fileId <> "/children" <>
        "?access_token=" <> accessToken tokens <> "&maxResults=1000"

    return $ case mlist of
        Just (ChildList items) -> items
        Nothing -> []
