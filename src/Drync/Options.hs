{-# LANGUAGE RecordWildCards #-}
module Drync.Options
    ( Options(..)
    , oProgress
    , getOptions
    ) where

import Options.Applicative
import System.Directory (getCurrentDirectory)

data Options = Options
    { oSyncFrom :: FilePath
    , oExcludes :: [String]
    , oProfile :: String
    , oRefresh :: Bool
    , oThrottle :: Int
    , oSilent :: Bool
    , oDebug :: Bool
    }

oProgress :: Options -> Int
oProgress Options{..} = if oSilent then 0 else 100

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
    <$> strOption
        (  short 'f'
        <> long "sync-from"
        <> metavar "DIR"
        <> value cwd
        <> help "Sync from the given directory"
        )
    <*> many (strOption
        (  short 'x'
        <> long "exclude"
        <> metavar "PATTERN"
        <> help "Exclude files and folders matching PATTERN"
        ))
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
