{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.Monoid ((<>))
import Data.Text (Text)
import System.Environment.XDG.BaseDir (getUserCacheDir)
import System.FilePath ((</>))
import System.IO (hPutStrLn, stderr)

import Drync.Client
import Drync.Token

import Drync.Drive.Api
import Drync.Drive.Item

import qualified Data.Text.IO as T

main :: IO ()
main = do
    let profile = "default" -- TODO: options

    dir <- getUserCacheDir appName
    tokens <- generateTokens False client $ dir </> profile <> ".token"

    showDirectory tokens "/" root

showDirectory :: OAuth2Tokens -> Text -> FileId -> IO ()
showDirectory tokens parent fileId = do
    mitem <- getFile tokens fileId

    case mitem of
        Nothing -> return ()
        Just item -> do
            let title = itemTitle item
                filename = parent <> title

            T.putStrLn filename

            children <- getChildren tokens fileId

            mapM_ (showDirectory tokens (filename <> "/")) children

appName :: String
appName = "drync"

err :: String -> IO ()
err = hPutStrLn stderr
