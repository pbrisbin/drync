{-# LANGUAGE OverloadedStrings #-}
module Drync.Options
    ( Options(..)
    , getOptions
    ) where

import Data.Text (Text)

data Options = Options
    { oProfile :: String
    , oSyncFrom :: FilePath
    , oSyncTo :: Text
    }

-- Sample options for testing
getOptions :: IO Options
getOptions = return Options
    { oProfile = "default"
    , oSyncFrom = "/home/patrick/Downloads"
    , oSyncTo = "Downloads"
    }
