module Drync.Drive.Api.Search
    ( Query(..)
    , getFiles
    ) where

import Data.ByteString (ByteString)
import Data.Monoid ((<>))
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)

import Drync.Drive.Api.File
import Drync.Drive.Api.HTTP

data Query
    = TitleEq Text
    | ParentEq FileId
    | Query `And` Query
    | Query `Or` Query

getFiles :: Query -> Api [Item]
getFiles query = do
    let query' =
            [ ("q", Just $ toParam query)
            , ("maxResults", Just "1000")
            ]

    mlist <- getApi "/files" query'

    return $ case mlist of
        Just (Items items) -> unTrashed items
        Nothing -> []

  where
    unTrashed :: [Item] -> [Item]
    unTrashed = filter (not . itemTrashed)

toParam :: Query -> ByteString
toParam (TitleEq title) = "title = " <> quote title
toParam (ParentEq fileId) = quote fileId <> " in parents"
toParam (p `And` q) = "(" <> toParam p <> ") and (" <> toParam q <> ")"
toParam (p `Or` q) = "(" <> toParam p <> ") or (" <> toParam q <> ")"

quote :: Text -> ByteString
quote = ("'" <>) . (<> "'") . encodeUtf8
