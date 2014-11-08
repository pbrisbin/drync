module Data.Conduit.Throttle
    ( throttle
    ) where

import Control.Concurrent (threadDelay)
import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Conduit (Conduit, await, yield)
import Data.Time (UTCTime, diffUTCTime, getCurrentTime)

type Units = Int

throttle :: MonadIO m
         => (e -> Units) -- ^ How many units per element?
         -> Units        -- ^ Limit to this many per second
         -> Conduit e m e
throttle len limit = loop 0 =<< liftIO getCurrentTime

  where
    loop c start = do
        mv <- await

        case mv of
            Nothing -> return ()
            Just v -> do
                let total = c + len v

                liftIO $ limitSpeed limit total start

                yield v

                loop total start

limitSpeed :: Units -> Units -> UTCTime -> IO ()
limitSpeed limit total start = do
    now <- getCurrentTime

    let elapsed = realToFrac $ diffUTCTime now start :: Double
        speed = fromIntegral total / elapsed

    when (speed > fromIntegral limit) $ do
        threadDelay $ 100 * 1000 -- block 0.1 seconds and check again
        limitSpeed limit total start

    return ()
