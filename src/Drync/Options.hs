module Drync.Options
    ( Options(..)
    , getOptions
    ) where

import Data.Text (Text)
import Options.Applicative
import System.FilePath (takeFileName)
import System.Directory (getCurrentDirectory)

import qualified Data.Text as T

data Options = Options
    { oProfile :: String
    , oRefresh :: Bool
    , oSyncFrom :: FilePath
    , oSyncTo :: Text
    }

getOptions :: IO Options
getOptions = do
    cwd <- getCurrentDirectory

    execParser $ parseOptions cwd `withInfo`
        "Sync a local directory with one on Google Drive"

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
    <*> fmap T.pack (strOption
        (  short 't'
        <> long "sync-to"
        <> metavar "FOLDER"
        <> value (takeFileName cwd)
        <> help "Sync to the given folder"
        ))
