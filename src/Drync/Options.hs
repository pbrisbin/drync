{-# LANGUAGE RecordWildCards #-}
module Drync.Options
    ( Options(..)
    , oProgress
    , message
    , messageDebug
    , getOptions
    ) where

import Options.Applicative

import Control.Monad (unless, when)
import System.Directory (getCurrentDirectory)
import System.FilePath.Glob (Pattern, compile)
import System.IO (hPutStrLn, stderr)

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

oProgress :: Options -> Int
oProgress Options{..} = if oSilent then 0 else 100

message :: Options -> String -> IO ()
message options = unless (oSilent options) . putStrLn

messageDebug :: Options -> String -> IO ()
messageDebug options = when (oDebug options) . hPutStrLn stderr

getOptions :: IO Options
getOptions = do
    cwd <- getCurrentDirectory

    execParser $ parseOptions cwd `withInfo`
        "Sync a local directory with Google Drive"

  where
    withInfo :: Parser a -> String -> ParserInfo a
    withInfo opts desc = info (helper <*> opts) $ progDesc desc

parseOptions :: FilePath -> Parser Options
parseOptions cwd = Options
    <$> argument str (metavar "DIRECTORY" <> value cwd)
    <*> many (compile <$> strOption
        (  short 'x'
        <> long "exclude"
        <> metavar "PATTERN"
        <> help "Exclude files and folders matching PATTERN"
        ))
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
        <> value "default"
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
        <> value 0
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
