module Drync.Missing
    ( missingLocal
    , missingRemote
    ) where

import Drync.Api
import Drync.Transfer
import Drync.Options
import Drync.System

import Data.Monoid ((<>))
import Network.Google.Drive
import System.Directory
    ( createDirectoryIfMissing
    , doesDirectoryExist
    , removeDirectoryRecursive
    , removeFile
    )
import System.FilePath ((</>))

missingLocal :: Options -> FilePath -> File -> Api ()
missingLocal options parent remote =
    if oDeleteRemote options
        then deleteRemote options remote
        else createLocal options parent remote

missingRemote :: Options -> File -> FilePath -> Api ()
missingRemote options parent local =
    if oDeleteLocal options
        then deleteLocal options local
        else createRemote options parent local

createLocal :: Options -> FilePath -> File -> Api ()
createLocal options parent remote = do
    let local = parent </> localPath remote

    if not $ isFolder remote
        then download options remote local
        else do
            liftIO $ do
                message options local
                createDirectoryIfMissing False local

            mapM_ (createLocal options local) =<< listChildren remote

createRemote :: Options -> File -> FilePath -> Api ()
createRemote options parent local = do
    remote <- newFile (fileId parent) local
    isDirectory <- liftIO $ doesDirectoryExist local

    if not $ isDirectory
        then upload options local remote
        else do
            liftIO $ message options $ show remote

            folder <- createAsFolder remote
            children <- liftIO $ getVisibleDirectoryContents local

            mapM_ (createRemote options folder) $ map (local </>) children

deleteRemote :: Options -> File -> Api ()
deleteRemote options file = do
    liftIO $ message options $ "delete " <> show file

    deleteFile file

deleteLocal :: Options -> FilePath -> Api ()
deleteLocal options local = liftIO $ do
    message options $ "delete " <> local

    isDirectory <- doesDirectoryExist local

    if isDirectory
        then removeDirectoryRecursive local
        else removeFile local
