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

-- | Find a file by Id
getFile :: OAuth2Tokens -> FileId -> IO (Maybe Item)
getFile tokens fileId = fmap decode $ simpleHttp $ baseUrl <>
    "/files/" <> T.unpack fileId <> "?access_token=" <> accessToken tokens

-- | Find files matching the given query, limited to 1000 results
getFiles :: OAuth2Tokens -> Query -> IO [Item]
getFiles tokens query = do
    mlist <- fmap decode $ simpleHttp $ baseUrl <>
        "/files" <> "?access_token=" <> accessToken tokens <>
        "&q=" <> toParam query <> "&maxResults=1000"

    return $ case mlist of
        Just (Items items) -> unTrashed items
        Nothing -> []

-- | Return all child Items of the given folder
getFilesByParent :: OAuth2Tokens -> FileId -> IO [Item]
getFilesByParent tokens = getFiles tokens . ParentEq

-- TODO: handle abiguities since this just searches by name
getFolder :: OAuth2Tokens -> Text -> IO (Maybe Item)
getFolder tokens path = do
    folders <- getFiles tokens $ TitleEq path

    when (length folders > 1) $
        putStrLn "warning: folder name returned multiple results"

    return $ listToMaybe folders
