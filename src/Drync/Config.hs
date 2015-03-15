module Drync.Config
    ( Config(..)
    , readConfig
    ) where

import Control.Applicative ((<$>), (<*>))
import System.Directory (doesFileExist, getCurrentDirectory)

import qualified Data.Configurator as C
import qualified Data.Configurator.Types as C

data Config = Config
    { cSyncFrom :: FilePath
    , cProfile :: String
    , cThrottle :: Int
    }

readConfig :: FilePath -> IO Config
readConfig fp = do
    exists <- doesFileExist fp

    toConfig =<< if exists
        then C.load [C.Required fp]
        else return C.empty

toConfig :: C.Config -> IO Config
toConfig conf = do
    cwd <- getCurrentDirectory

    Config
        <$> C.lookupDefault cwd conf "sync_from"
        <*> C.lookupDefault "default" conf "profile"
        <*> C.lookupDefault 0 conf "throttle"
