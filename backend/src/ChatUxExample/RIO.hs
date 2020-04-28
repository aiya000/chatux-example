module ChatUxExample.RIO where

import RIO
import Tonatona (HasConfig(..), HasParser(..))
import qualified Tonatona.Logger as TonaLogger

newtype Config = Config
  { tonaLogger :: TonaLogger.Config
  }

instance HasConfig Config TonaLogger.Config where
  config = tonaLogger

instance HasParser Config where
  parser = Config <$> parser


logInfo :: (MonadIO m, MonadReader Config m) => Utf8Builder -> m ()
logInfo = liftRIO . TonaLogger.logInfo

logDebug :: (MonadIO m, MonadReader Config m) => Utf8Builder -> m ()
logDebug = liftRIO . TonaLogger.logDebug

logError :: (MonadIO m, MonadReader Config m) => Utf8Builder -> m ()
logError = liftRIO . TonaLogger.logError
