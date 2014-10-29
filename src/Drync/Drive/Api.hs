-- |
--
-- TODO: Exception handling
--
module Drync.Drive.Api
    ( getFiles
    ) where

import Data.Aeson (decode)
import Data.Monoid ((<>))
import Network.HTTP.Conduit (simpleHttp)

import Drync.Token
import Drync.Drive.FileList

baseUrl :: String
baseUrl = "https://www.googleapis.com/drive/v2"

-- | Get the full FileList from Google Drive
getFiles :: OAuth2Tokens -> IO (Maybe FileList)
getFiles tokens = fmap decode $ simpleHttp $
    baseUrl <> "/files" <> "?access_token=" <> accessToken tokens
