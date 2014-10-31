module Main where

import System.Environment.XDG.BaseDir (getUserCacheDir)
import System.FilePath ((</>), (<.>))

import Drync.Client
import Drync.Options
import Drync.Sync
import Drync.Token

import Drync.Drive.Api

appName :: String
appName = "drync"

main :: IO ()
main = do
    options <- getOptions

    file <- tokenFile $ oProfile options
    tokens <- generateTokens False client file

    let path = oSyncFrom options
        query = TitleEq (oSyncTo options) `And` ParentEq "root"

    runApi tokens $ do
        -- TODO: error handling
        (item:_) <- getFiles query

        executeSync (Sync path item)

tokenFile :: String -> IO FilePath
tokenFile profile = do
    cdir <- getUserCacheDir appName
    return $ cdir </> profile <.> "token"
