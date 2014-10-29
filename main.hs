{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Main where

import Data.Monoid ((<>))
import Data.Text (Text)
import System.Directory (getCurrentDirectory)
import System.Environment.XDG.BaseDir (getUserCacheDir)
import System.FilePath ((</>))

import Drync.Client
import Drync.Token
import Drync.Drive.Api
import Drync.Drive.Item

main :: IO ()
main = do
    -- TODO: options
    let profile = "default"
        folder = root

    cdir <- getUserCacheDir appName
    tokens <- generateTokens False client $ cdir </> profile <> ".token"

    mtop <- getFile tokens folder

    case mtop of
        Nothing -> return ()
        Just top -> syncFolder tokens "/" top

syncFolder :: OAuth2Tokens -> Text -> Item -> IO ()
syncFolder tokens prefix Item{..} = do
    let title = prefix <> itemTitle
        prefix' = title <> "/"

    print title

    mapM_ (syncFolder tokens prefix') =<< getChildren tokens itemId

appName :: String
appName = "drync"
