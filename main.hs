module Main where

import Network.Google.OAuth2
import System.Environment.XDG.BaseDir (getUserCacheDir)
import System.FilePath ((</>), (<.>))

import qualified Data.Text as T

import Drync.Client
import Drync.Options
import Drync.Sync

main :: IO ()
main = do
    options <- getOptions

    file <- tokenFile $ oProfile options
    tokens <- generateTokens client scopes (oRefresh options) file

    sync tokens (oSyncFrom options) $ T.pack $ oSyncTo options

appName :: String
appName = "drync"

scopes :: [OAuth2Scope]
scopes = ["https://www.googleapis.com/auth/drive"]

tokenFile :: String -> IO FilePath
tokenFile profile = do
    cdir <- getUserCacheDir appName
    return $ cdir </> profile <.> "token"
