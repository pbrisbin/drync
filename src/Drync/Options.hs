module Drync.Options
    ( Options(..)
    , getOptions
    ) where

import Data.List (dropWhileEnd)
import Options.Applicative
import System.Directory (getCurrentDirectory)
import System.FilePath (takeFileName, isPathSeparator)

data Options = Options
    { oProfile :: String
    , oRefresh :: Bool
    , oSyncFrom :: FilePath
    , oSyncTo :: String
    , oMultipart :: Bool
    , oThrottle :: Int
    , oProgress :: Int
    , oDebug :: Bool
    }

getOptions :: IO Options
getOptions = do
    cwd <- getCurrentDirectory
    options <- execParser $ parseOptions cwd `withInfo`
        "Sync a local directory with one on Google Drive"

    let syncTo = oSyncTo options <|> baseDirectory (oSyncFrom options)

    return options { oSyncTo = syncTo }

  where
    withInfo :: Parser a -> String -> ParserInfo a
    withInfo opts desc = info (helper <*> opts) $ progDesc desc

    baseDirectory :: FilePath -> FilePath
    baseDirectory = takeFileName . dropWhileEnd isPathSeparator

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
    <*> strOption
        (  short 't'
        <> long "sync-to"
        <> metavar "FOLDER"
        <> value ""
        <> help "Sync to the given folder"
        )
    <*> switch
        (  short 'm'
        <> long "multipart"
        <> help "Use multipart uploads instead of resumable"
        )
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
