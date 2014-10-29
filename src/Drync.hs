{-# LANGUAGE OverloadedStrings #-}
module Drync (drync) where

import Control.Applicative ((<$>))
import Data.Monoid ((<>))
import System.Directory -- TODO imports

import qualified Data.Text as T
import qualified Data.Text.IO as T

import Drync.Token
import Drync.Drive.Api
import Drync.Drive.Item

drync :: OAuth2Tokens -> FilePath -> Item -> IO ()
drync tokens path item = do
    T.putStrLn $
        "Syncing: " <> T.pack path <> " with " <> itemTitle item <> "..."

    files <- filter (not . hidden) <$> getDirectoryContents path
    children <- getFilesByParent tokens $ itemId item

    let (local, both, remote) = categorize files children

    -- TODO: actually sync stuff and recurse
    mapM_ (\f -> T.putStrLn $ "  UPLOAD: " <> T.pack f) local
    mapM_ (\i -> T.putStrLn $ "DOWNLOAD: " <> itemTitle i) remote
    mapM_ (\i -> T.putStrLn $ "    SYNC: " <> itemTitle i) both

  where
    hidden :: FilePath -> Bool
    hidden ('.':_) = True
    hidden _ = False

-- This has to be horribly inefficient, but hopefully the lists are small enough
-- that it doesn't really matter.
categorize :: [FilePath] -> [Item] -> ([FilePath], [Item], [Item])
categorize paths items = (local, both, remote)
  where
    both = filter ((`elem` paths) . T.unpack . itemTitle) items
    local = filter ((`notElem` (map itemTitle both)) . T.pack) paths
    remote = filter (`notElem` both) items
