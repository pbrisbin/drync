module Drync.Drive.SyncPlan
    ( processRemoteFiles
    , processLocalFiles
    , createSyncPlan
    , printSyncPlan
    , executeSyncPlan
    ) where

import Control.Monad.State

import Drync.Token
import Drync.Drive.Item

data Action = Action

type Plan = [Action]

type SyncPlan a = State Plan a

processRemoteFiles :: OAuth2Tokens -> FileId -> SyncPlan ()
processRemoteFiles = undefined

processLocalFiles :: FilePath -> SyncPlan ()
processLocalFiles = undefined

createSyncPlan :: SyncPlan a -> IO Plan
createSyncPlan = undefined

printSyncPlan :: Plan -> IO ()
printSyncPlan = undefined

executeSyncPlan :: Plan -> IO ()
executeSyncPlan = undefined
