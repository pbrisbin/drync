{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Drync.Drive.FileListSpec (main, spec) where

import Test.Hspec
import Drync.Drive.FileList

import Data.Aeson (decode)
import qualified Data.ByteString.Lazy as BS

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "FileList" $ do
        it "decodes correctly" $ do
            Just (FileList items) <- fmap decode $ BS.readFile "test/files.json"

            map toItemIds items `shouldBe`
                [ ("0B14QfGpVCT4KamR4cmp4Wnl2XzQ", [])
                , ("0B93xZ4-ZE27INnIxSU5iRXlUVWM", ["0B14QfGpVCT4KamR4cmp4Wnl2XzQ"])
                , ("0B14QfGpVCT4KdEwxbWp2Y0ZaNTVGMHJMNmY4SzlGallKVGxB", ["0B93xZ4-ZE27INnIxSU5iRXlUVWM"])
                , ("0B93xZ4-ZE27IdlJsUDUwRGJoNDA", ["0B93xZ4-ZE27ITVV0MEhwMlRkTmc"])
                , ("0B93xZ4-ZE27INGcxMU1VamtBbVE", ["0B93xZ4-ZE27ITVV0MEhwMlRkTmc"])
                ]
  where
    toItemIds :: Item -> (ItemId, [ItemId])
    toItemIds Item{..} = (itemId, itemParents)
