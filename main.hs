module Main where

import Network.Google.Api
import Network.Google.OAuth2
import System.Environment.XDG.BaseDir (getUserCacheDir)
import System.FilePath ((</>), (<.>))
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)

import Drync
import Drync.Client
import Drync.Options

main :: IO ()
main = do
    options <- getOptions

    file <- tokenFile $ oProfile options
    token <- getAccessToken client scopes (oRefresh options) file

    -- TODO: Find root or nested folder
    let syncTo = undefined

    result <- runApi token (oDebug options) $ sync (oSyncFrom options) syncTo

    case result of
        Right _ -> return ()
        Left ex -> do
            hPutStrLn stderr $ show ex
            exitFailure

appName :: String
appName = "drync"

scopes :: [OAuth2Scope]
scopes = ["https://www.googleapis.com/auth/drive"]

tokenFile :: String -> IO FilePath
tokenFile profile = do
    cdir <- getUserCacheDir appName
    return $ cdir </> profile <.> "token"
