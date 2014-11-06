module Main where

import Data.Monoid ((<>))
import Network.Google.Drive.Api
import Network.Google.OAuth2
import System.Environment.XDG.BaseDir (getUserCacheDir)
import System.FilePath ((</>), (<.>))
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)

import qualified Data.Text as T

import Drync.Client
import Drync.Options
import Drync.Sync

main :: IO ()
main = do
    options <- getOptions

    file <- tokenFile $ oProfile options
    token <- getAccessToken client scopes (oRefresh options) file

    let from = oSyncFrom options
        to = T.pack $ oSyncTo options

    result <- runApi token $ sync from to

    case result of
        Left ex -> do
            hPutStrLn stderr $ "error: " <> show ex
            exitFailure

        _ -> return ()

appName :: String
appName = "drync"

scopes :: [OAuth2Scope]
scopes = ["https://www.googleapis.com/auth/drive"]

tokenFile :: String -> IO FilePath
tokenFile profile = do
    cdir <- getUserCacheDir appName
    return $ cdir </> profile <.> "token"
