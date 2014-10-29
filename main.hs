{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.Monoid ((<>))
import Data.Text (Text)
import System.Environment.XDG.BaseDir (getUserCacheDir)
import System.FilePath ((</>))

import Drync
import Drync.Client
import Drync.Token
import Drync.Drive.Api

data Options = Options
    { oProfile :: String
    , oSyncFrom :: FilePath
    , oSyncTo :: Text
    }

main :: IO ()
main = do
    options <- getOptions

    file <- tokenFile $ oProfile options
    tokens <- generateTokens False client file

    let run = drync tokens $ oSyncFrom options

    maybe notFound run =<< getFolder tokens (oSyncTo options)

-- Sample options for testing
getOptions :: IO Options
getOptions = return Options
    { oProfile = "default"
    , oSyncFrom = "/home/patrick/Downloads"
    , oSyncTo = "Downloads"
    }

appName :: String
appName = "drync"

tokenFile :: String -> IO FilePath
tokenFile profile = do
    cdir <- getUserCacheDir appName
    return $ cdir </> profile <> ".token"

notFound :: IO ()
notFound = putStrLn "error: remote sync-to folder not found"
