{-# LANGUAGE TypeApplications #-}

module Housekeeping.Prelude (module RIO, logInfo, logWarn, logDebug, logError) where

import Control.Env.Hierarchical
import RIO hiding (logDebug, logError, logInfo, logWarn)
import qualified RIO

logInfo, logWarn, logDebug, logError :: (MonadIO m, MonadReader env m, Has LogFunc env, HasCallStack) => Utf8Builder -> m ()
logInfo s = do
  logFunc <- view (getL @LogFunc)
  runRIO logFunc $ RIO.logInfo s
logWarn s = do
  logFunc <- view (getL @LogFunc)
  runRIO logFunc $ RIO.logWarn s
logDebug s = do
  logFunc <- view (getL @LogFunc)
  runRIO logFunc $ RIO.logDebug s
logError s = do
  logFunc <- view (getL @LogFunc)
  runRIO logFunc $ RIO.logError s