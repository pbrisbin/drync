module Main where

import Data.Monoid ((<>))
import System.Environment.XDG.BaseDir (getUserCacheDir)
import System.FilePath ((</>))
import qualified Data.ByteString.Lazy.Char8 as BL

import Drync.Client
import Drync.Request
import Drync.Token

main :: IO ()
main = do
    let profile = "default" -- TODO: options

    dir <- getUserCacheDir appName
    tokens <- generateTokens False client $ dir </> profile <> ".token"

    BL.putStrLn =<< driveFiles tokens

appName :: String
appName = "drync"
