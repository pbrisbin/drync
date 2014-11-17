module Drync.Options
    ( Options(..)
    , getOptions
    ) where

import Options.Applicative
import System.Directory (getCurrentDirectory)

data Options = Options
    { oProfile :: String
    , oRefresh :: Bool
    , oSyncFrom :: FilePath
    , oExcludes :: [String]
    , oThrottle :: Int
    , oProgress :: Int
    , oDebug :: Bool
    }

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
    <*> strOption
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
    <*> option auto
        (  short 'T'
        <> long "throttle"
        <> metavar "N"
        <> value 0
        <> help "Throttle HTTP to N KB/s"
        )
    <*> option auto
        (  short 'P'
        <> long "progress"
        <> metavar "N"
        <> value 100
        <> help "Output transfer progress every N bytes"
        )
    <*> switch
        (  short 'd'
        <> long "debug"
        <> help "Output debugging messages"
        )
