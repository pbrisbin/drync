module Drync.Missing
    ( missingLocal
    , missingRemote
    ) where

import Drync.Api
import Drync.Transfer
import Drync.Options
import Drync.System

import Network.Google.Drive
import System.Directory (createDirectoryIfMissing, doesDirectoryExist)
import System.FilePath ((</>))

missingLocal :: Options -> FilePath -> File -> Api ()
missingLocal options parent remote = do
    let local = parent </> localPath remote

    if not $ isFolder remote
        then download options remote local
        else do
            liftIO $ do
                message options local
                createDirectoryIfMissing False local

            mapM_ (missingLocal options local) =<< listChildren remote

missingRemote :: Options -> File -> FilePath -> Api ()
missingRemote options parent local = do
    remote <- newFile (fileId parent) local
    isDirectory <- liftIO $ doesDirectoryExist local

    if not $ isDirectory
        then upload options local remote
        else do
            liftIO $ message options $ show remote

            folder <- createAsFolder remote
            mapM_ (missingRemote options folder) =<<
                liftIO (getVisibleDirectoryContents local)
