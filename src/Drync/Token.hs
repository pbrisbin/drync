module Drync.Token
    ( OAuth2Tokens(..)
    , generateTokens
    ) where

import Control.Monad (void)
import Data.Monoid ((<>))
import Network.Google.OAuth2

import qualified Control.Exception as E

-- | Find or generate refreshed OAuth2 tokens.
generateTokens :: Bool            -- ^ Ignore cache?
               -> OAuth2Client    -- ^ Client to user
               -> FilePath        -- ^ File in which to cache the token
               -> IO OAuth2Tokens -- ^ Refreshed token
generateTokens force client tokenFile = do
    tokens <- if force
        then newTokens client
        else fromMaybeM (newTokens client) (cachedTokens tokenFile)

    refreshed <- refreshTokens client tokens

    void $ cacheTokens tokenFile refreshed

    return refreshed

newTokens :: OAuth2Client -> IO OAuth2Tokens
newTokens client = do
    let permissionUrl = formUrl client ["https://www.googleapis.com/auth/drive"]

    putStrLn $ rule 80
    putStrLn "Please visit the following URL to retrieve a verification code:"
    putStrLn ""
    putStrLn $ "  " <> permissionUrl
    putStrLn ""
    putStr "Enter your verification code: "
    code <- getLine
    putStrLn $ rule 80

    exchangeCode client code

  where
    rule :: Int -> String
    rule n = map (const '-') [1..n]

cachedTokens :: FilePath -> IO (Maybe OAuth2Tokens)
cachedTokens tokenFile = do
    result <- fmap (fmap reads) $ try $ readFile tokenFile

    return $ case result of
        Right ((t,_):_) -> Just t
        _ -> Nothing

cacheTokens :: FilePath -> OAuth2Tokens -> IO OAuth2Tokens
cacheTokens tokenFile t = fmap (const t) $ try $ writeFile tokenFile (show t)

fromMaybeM :: Monad m => m b -> m (Maybe b) -> m b
fromMaybeM mb ma = ma >>= maybe mb return

try :: IO a -> IO (Either E.IOException a)
try = E.try
