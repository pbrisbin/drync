module Main where

import Data.Monoid ((<>))
import System.Environment.XDG.BaseDir (getUserCacheDir)
import System.FilePath ((</>))

import Drync
import Drync.Client
import Drync.Options
import Drync.Token
import Drync.Drive.Api

main :: IO ()
main = do
    options <- getOptions

    file <- tokenFile $ oProfile options
    tokens <- generateTokens False client file

    let run = drync tokens $ oSyncFrom options

    maybe notFound run =<< getFolder tokens (oSyncTo options)

tokenFile :: String -> IO FilePath
tokenFile profile = do
    cdir <- getUserCacheDir appName
    return $ cdir </> profile <> ".token"

appName :: String
appName = "drync"

notFound :: IO ()
notFound = putStrLn "error: remote sync-to folder not found"
