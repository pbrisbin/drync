{-# LANGUAGE QuasiQuotes #-}
module Drync.Client (client) where

import Drync.Client.TH
import Network.Google.OAuth2 (OAuth2Client(..))

client :: OAuth2Client
client = OAuth2Client
    { clientId = cid
    , clientSecret = cs
    }

  where
    (cid, cs) = [credentials|.env|]
