module Main where

import Network.Google.Api
import Network.Google.OAuth2
import System.Environment.XDG.BaseDir
import System.Exit
import System.FilePath
import System.IO

import Drync
import Drync.Client
import Drync.Options

-- TODO
--
-- * support --sync-to again
-- * support --throttle again
-- * support --progress again
-- * support --exclude
--
main :: IO ()
main = do
    options <- getOptions

    mfile <- if oRefresh options
        then return Nothing
        else fmap Just $ tokenFile $ oProfile options

    token <- getAccessToken client scopes mfile
    syncTo <- getFile "root"
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
