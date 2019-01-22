{-# LANGUAGE GADTs        #-}
{-# LANGUAGE TypeFamilies #-}
module Lodjur.Output.OutputLoggers
  ( OutputLoggers (..)
  , OutputLoggersMessage (..)
  , initialize
  ) where

import           Lodjur.Database            (DbPool)
import           Lodjur.Deployment          (JobId)
import qualified Lodjur.Output.Database     as Database
import           Lodjur.Output.OutputLogger (OutputLogger)
import qualified Lodjur.Output.OutputLogger as OutputLogger
import           Lodjur.Process

newtype OutputLoggers = OutputLoggers DbPool

initialize :: DbPool -> IO OutputLoggers
initialize pool = do
  Database.initialize pool
  return (OutputLoggers pool)

data OutputLoggersMessage r where
  -- Public messages:
  SpawnOutputLogger :: JobId -> OutputLoggersMessage (Sync (Ref OutputLogger))

instance Process OutputLoggers where
  type Message OutputLoggers = OutputLoggersMessage

  receive _self (a@(OutputLoggers pool), SpawnOutputLogger jobId) = do
    logger <- spawn =<< OutputLogger.initialize pool jobId
    return (a, logger)

  terminate _ = return ()
