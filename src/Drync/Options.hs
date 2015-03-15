{-# LANGUAGE RecordWildCards #-}
module Drync.Options
    ( Options(..)
    , message
    , messageDebug
    , getOptions
    , readExcludes
    ) where

import Options.Applicative

import Control.Monad (unless, when)
import Data.List (isPrefixOf)
import System.Directory (doesFileExist)
import System.FilePath.Glob (Pattern, compile)
import System.IO (hPutStrLn, stderr)

import Drync.Config

data Options = Options
    { oSyncFrom :: FilePath
    , oExcludes :: [Pattern]
    , oDeleteLocal :: Bool
    , oDeleteRemote :: Bool
    , oProfile :: String
    , oRefresh :: Bool
    , oThrottle :: Int
    , oSilent :: Bool
    , oDebug :: Bool
    }

message :: Options -> String -> IO ()
message options = unless (oSilent options) . putStrLn

messageDebug :: Options -> String -> IO ()
messageDebug options = when (oDebug options) . hPutStrLn stderr

getOptions :: Config -> [Pattern] -> IO Options
getOptions conf excludes =
    execParser $ parseOptions conf excludes `withInfo`
        "Sync a local directory with Google Drive"

  where
    withInfo :: Parser a -> String -> ParserInfo a
    withInfo opts desc = info (helper <*> opts) $ progDesc desc

parseOptions :: Config -> [Pattern] -> Parser Options
parseOptions Config{..} excludes = Options
    <$> argument str (metavar "DIRECTORY" <> value cSyncFrom)
    <*> fmap (excludes ++) (many (compile <$> strOption
        (  short 'x'
        <> long "exclude"
        <> metavar "PATTERN"
        <> help "Exclude files and folders matching PATTERN"
        )))
    <*> switch
        (  long "delete-local"
        <> help "Delete files which exist only locally"
        )
    <*> switch
        (  long "delete-remote"
        <> help "Delete files which exist only on your Drive"
        )
    <*> strOption
        (  short 'p'
        <> long "profile"
        <> metavar "NAME"
        <> value cProfile
        <> help "Use the named profile"
        )
    <*> switch
        (  short 'r'
        <> long "refresh-oauth"
        <> help "Ignore cached OAuth2 credentials"
        )
    <*> option auto
        (  short 't'
        <> long "throttle"
        <> metavar "N"
        <> value cThrottle
        <> help "Throttle HTTP to N KB/s"
        )
    <*> switch
        (  short 's'
        <> long "silent"
        <> help "Output nothing beyond errors"
        )
    <*> switch
        (  short 'd'
        <> long "debug"
        <> help "Output debugging messages"
        )

readExcludes :: FilePath -> IO [Pattern]
readExcludes fp = do
    exists <- doesFileExist fp
    content <- if exists then readFile fp else return ""

    return $ map compile $ filter (not . comment) $ lines content

  where
    comment = ("#" `isPrefixOf`)
