module Drync.Drive.Api.HTTP
    ( Api
    , Path
    , Params
    , runApi
    , simpleApi
    , getApi
    , postApi

    -- Re-exports
    , liftIO
    ) where

import Control.Monad.Reader
import Data.Aeson --(decode, encode)
import Data.ByteString (ByteString)
import Data.Monoid ((<>))
import Network.HTTP.Conduit --(Request(..), RequestBody(..), parseUrl, simpleHttp)
--import Network.HTTP.Types (Headers, hAuthorization, hContentType)

import qualified Data.ByteString.Char8 as C8

import Drync.Token

type Api a = ReaderT OAuth2Tokens IO a

runApi :: OAuth2Tokens -> Api a -> IO a
runApi tokens f = runReaderT f tokens

type Path = String
type Params = [(ByteString, Maybe ByteString)]

baseUrl :: String
baseUrl = "https://www.googleapis.com/drive/v2"

simpleApi :: FromJSON a => Path -> Api (Maybe a)
simpleApi path = getApi path []

getApi :: FromJSON a => Path -> Params -> Api (Maybe a)
getApi path query = do
    request <- withToken query =<< liftIO (parseUrl $ baseUrl <> path)

    fmap (decode . responseBody) $ withManager $ httpLbs request

postApi :: (ToJSON a, FromJSON b) => Path -> a -> Api (Maybe b)
postApi = undefined
-- createFolder :: FileId -> Text -> Api (Maybe Item)
-- createFolder parentId folder = do
--     request' <- parseUrl $ baseUrl <> "/files"

--     let
--         request = addHeaders headers $ request'
--             { method = "POST"
--             , requestBody = RequestBodyLBS $ encode body
--             }

--     return Nothing -- TODO

--   where
--     headers :: Headers
--     headers =
--             [ (hAuthorization, "Bearer " <> show (accessToken tokens))
--             , (hContentType, "application/json")
--             ]

withToken :: Params -> Request -> Api Request
withToken query request = do
    tokens <- ask

    let token = C8.pack $ accessToken tokens
    let query' = ("access_token", Just token):query

    return $ setQueryString query' request
