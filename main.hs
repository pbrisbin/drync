{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.Monoid ((<>))
import System.Directory (getCurrentDirectory)
import System.Environment.XDG.BaseDir (getUserCacheDir)
import System.FilePath ((</>))

import Drync.Client
import Drync.Token
import Drync.Drive.Item
import Drync.Drive.SyncPlan

main :: IO ()
main = do
    -- TODO: options
    let profile = "default"
        directory = root
        debug = True

    dir <- getCurrentDirectory
    cdir <- getUserCacheDir appName
    tokens <- generateTokens False client $ cdir </> profile <> ".token"

    plan <- createSyncPlan $ do
        processRemoteFiles tokens directory
        processLocalFiles dir

    if debug
        then printSyncPlan plan
        else executeSyncPlan plan

appName :: String
appName = "drync"
