{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE TypeFamilies               #-}
module Lodjur.OutputLogger
  ( Output (..)
  , OutputLogs
  , OutputLogger
  , OutputLogMessage (..)
  , initialize
  ) where

import           Data.Time.Clock

import qualified Lodjur.Database        as Database
import           Lodjur.Deployment
import           Lodjur.Process

newtype OutputLogger = OutputLogger Database.DbPool

initialize :: Database.DbPool -> IO OutputLogger
initialize pool = return (OutputLogger pool)

data OutputLogMessage r where
  -- Public messages:
  AppendOutput :: JobId -> [String] -> OutputLogMessage Async
  GetOutputLogs :: OutputLogMessage (Sync OutputLogs)

instance Process OutputLogger where
  type Message OutputLogger = OutputLogMessage

  receive _self (a@(OutputLogger pool), AppendOutput jobid lines') = do
    now <- getCurrentTime
    Database.appendOutput pool jobid (Output now lines')
    return a

  receive _self (a@(OutputLogger pool), GetOutputLogs) = do
    logs <- Database.getAllOutputLogs pool
    return (a, logs)

  terminate (OutputLogger pool) = Database.destroyPool pool
