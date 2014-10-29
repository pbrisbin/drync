module Drync.Request
    ( driveFiles
    ) where

import Data.Monoid ((<>))
import Network.HTTP.Conduit (simpleHttp)

import Drync.Token

baseUrl :: String
baseUrl = "https://www.googleapis.com/drive/v2"

driveFiles tokens = simpleHttp $
    baseUrl <> "/files" <> "?access_token=" <> accessToken tokens
