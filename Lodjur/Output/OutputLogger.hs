{-# LANGUAGE GADTs        #-}
{-# LANGUAGE TypeFamilies #-}
module Lodjur.Output.OutputLogger
  ( Output (..)
  , OutputLogs
  , OutputLogger
  , OutputLogMessage (..)
  , initialize
  ) where

import           Data.Time.Clock

import           Lodjur.Database              (DbPool)
import           Lodjur.Deployment
import           Lodjur.Output
import qualified Lodjur.Output.Database as Database
import           Lodjur.Process

newtype OutputLogger = OutputLogger DbPool

initialize :: DbPool -> IO OutputLogger
initialize pool = do
  Database.initialize pool
  return (OutputLogger pool)

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

  terminate _ = return ()