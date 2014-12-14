-- |
--
-- TODO: Move these to google-drive
--
module Drync.Api
    ( listChildren
    , createAsFolder
    ) where

import Data.Monoid ((<>))
import Network.Google.Drive

listChildren :: File -> Api [File]
listChildren parent = listFiles $ ParentEq (fileId parent) `And` Untrashed

createAsFolder :: File -> Api File
createAsFolder file = do
    let fd = fileData file
    case fileParents fd of
        (parent:[]) -> createFolder parent $ fileTitle fd
        _ -> throwApiError errNoParent

  where
    errNoParent = "Cannot create folder without parent: " <> show file
