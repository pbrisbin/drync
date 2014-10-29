module Main where

import Data.Monoid ((<>))
import System.Environment.XDG.BaseDir (getUserCacheDir)
import System.FilePath ((</>))
import System.IO (hPutStrLn, stderr)

import Drync.Client
import Drync.Token

import Drync.Drive.Api
import Drync.Drive.FileList

main :: IO ()
main = do
    let profile = "default" -- TODO: options

    dir <- getUserCacheDir appName
    tokens <- generateTokens False client $ dir </> profile <> ".token"

    mlist <- getFiles tokens

    case mlist of
        Just (FileList items) -> print items
        Nothing -> err "Unable to download file list"

appName :: String
appName = "drync"

err :: String -> IO ()
err = hPutStrLn stderr
