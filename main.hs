module Main where

import System.Environment.XDG.BaseDir (getUserCacheDir)
import System.FilePath ((</>), (<.>))

import qualified Data.Text as T

import Drync.Client
import Drync.Options
import Drync.Sync
import Drync.Token

main :: IO ()
main = do
    options <- getOptions

    file <- tokenFile $ oProfile options
    tokens <- generateTokens (oRefresh options) client file

    sync tokens (oSyncFrom options) $ T.pack $ oSyncTo options

appName :: String
appName = "drync"

tokenFile :: String -> IO FilePath
tokenFile profile = do
    cdir <- getUserCacheDir appName
    return $ cdir </> profile <.> "token"
