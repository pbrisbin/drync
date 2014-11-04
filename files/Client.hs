module Drync.Client (client) where

import Network.Google.OAuth2 (OAuth2Client(..))

client :: OAuth2Client
client = OAuth2Client
    { clientId = "TODO"
    , clientSecret = "TODO"
    }
