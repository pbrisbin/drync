{-# LANGUAGE OverloadedStrings #-}
module Main where

import Network.Google.Api
import Network.Google.Drive.File
import Network.Google.OAuth2
import System.Environment.XDG.BaseDir
import System.FilePath

import Drync.Client
import Drync.Options
import Drync.Sync

main :: IO ()
main = do
    options <- getOptions

    mfile <- if oRefresh options
        then return Nothing
        else fmap Just $ tokenFile $ oProfile options

    token <- getAccessToken client scopes mfile

    runApi_ token $ do
        syncTo <- getFile "root"
        syncDirectory options (oSyncFrom options) syncTo

appName :: String
appName = "drync"

scopes :: [OAuth2Scope]
scopes = ["https://www.googleapis.com/auth/drive"]

tokenFile :: String -> IO FilePath
tokenFile profile = do
    cdir <- getUserCacheDir appName
    return $ cdir </> profile <.> "token"
