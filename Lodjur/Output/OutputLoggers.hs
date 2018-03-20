{-# LANGUAGE GADTs        #-}
{-# LANGUAGE TypeFamilies #-}
module Lodjur.Output.OutputLoggers
  ( OutputLoggers (..)
  , OutputLoggersMessage (..)
  , initialize
  ) where

import           Lodjur.Database            (DbPool)
import           Lodjur.Deployment          hiding (jobId)
import           Lodjur.Output.OutputLogger (OutputLogger)
import qualified Lodjur.Output.OutputLogger as OutputLogger
import           Lodjur.Process

newtype OutputLoggers = OutputLoggers DbPool

initialize :: DbPool -> IO OutputLoggers
initialize = return . OutputLoggers

data OutputLoggersMessage r where
  -- Public messages:
  SpawnOutputLogger :: JobId -> OutputLoggersMessage (Sync (Ref OutputLogger))

instance Process OutputLoggers where
  type Message OutputLoggers = OutputLoggersMessage

  receive _self (a@(OutputLoggers pool), SpawnOutputLogger jobId) = do
    logger <- spawn =<< OutputLogger.initialize pool jobId
    return (a, logger)

  terminate _ = return ()
