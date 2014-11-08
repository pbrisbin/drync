module Main where

import Data.Monoid ((<>))
import Network.Google.Api
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

    let syncFrom = oSyncFrom options
        syncTo = T.pack $ oSyncTo options
        apiOptions = def
            { apiUploadType =
                if oMultipart options
                    then Multipart
                    else Resumable
            , apiThrottle =
                if oThrottle options /= 0
                    then Just $ oThrottle options * 1000
                    else Nothing
            , apiProgress = Just $ oProgress options
            , apiDebug = oDebug options
            }

    result <- runApi token apiOptions $ sync syncFrom syncTo

    case result of
        Left ex -> do
            hPutStrLn stderr $ "API Error: " <> show ex
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
