{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE TypeFamilies               #-}
module Lodjur.OutputLogger
  ( Output
  , OutputLogs
  , OutputLogger
  , OutputLogMessage (..)
  , initialize
  ) where

import           Data.Time.Clock
import           Database.SQLite.Simple

import qualified Lodjur.Database        as Database
import           Lodjur.Deployment
import           Lodjur.Process

newtype OutputLogger = OutputLogger Connection

initialize :: Connection -> IO OutputLogger
initialize conn = return (OutputLogger conn)

data OutputLogMessage r where
  -- Public messages:
  AppendOutput :: JobId -> Output -> OutputLogMessage Async
  GetOutputLogs :: OutputLogMessage (Sync OutputLogs)

instance Process OutputLogger where
  type Message OutputLogger = OutputLogMessage

  receive _self (a@(OutputLogger conn), AppendOutput jobid output) = do
    now <- getCurrentTime
    Database.appendOutput conn now jobid output
    return a

  receive _self (a@(OutputLogger conn), GetOutputLogs) = do
    logs <- Database.getAllOutputLogs conn
    return (a, logs)

  terminate (OutputLogger conn) = close conn
