module Drync.Missing
    ( missingLocal
    , missingRemote
    ) where

import Drync.Transfer
import Drync.Options
import Drync.System

import Control.Applicative ((<$>), (<*>))
import Data.Monoid ((<>))
import Network.Google.Drive
import System.Directory
    ( createDirectoryIfMissing
    , doesDirectoryExist
    , getModificationTime
    , removeDirectoryRecursive
    , removeFile
    )
import System.FilePath ((</>), takeFileName)

import qualified Data.Foldable as F
import qualified Data.Text as T

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

            mapM_ (createLocal options local) =<< listVisibleContents remote

createRemote :: Options -> File -> FilePath -> Api ()
createRemote options parent local = do
    (modified, isDirectory) <- liftIO $ (,)
        <$> getModificationTime local
        <*> doesDirectoryExist local

    let fd = setParent parent $ newFile (T.pack $ takeFileName local) modified

    if not $ isDirectory
        then upload options local =<< createFile fd
        else do
            folder <- createFile $ setMimeType folderMimeType fd

            liftIO $ message options $ show folder

            children <- liftIO $ getVisibleDirectoryContents local

            mapM_ (createRemote options folder) $ map (local </>) children

deleteRemote :: Options -> File -> Api ()
deleteRemote options file = do
    liftIO $ message options $ "delete " <> show file

    -- We need to ensure we don't delete shared items. Since they'll never
    -- appear locally, passing --delete-remote would always delete them. One
    -- naive but so-far-accurate method is to confirm it has a Download URL
    -- before deleting.
    F.forM_ (fileDownloadUrl $ fileData file) $ \_ -> deleteFile file

deleteLocal :: Options -> FilePath -> Api ()
deleteLocal options local = liftIO $ do
    message options $ "delete " <> local

    isDirectory <- doesDirectoryExist local

    if isDirectory
        then removeDirectoryRecursive local
        else removeFile local
