{-# LANGUAGE GADTs             #-}
{-# LANGUAGE TypeFamilies      #-}
module Lodjur.EventLogger
  ( JobEvent (..)
  , EventLogs
  , EventLog
  , EventLogger
  , initialize
  , EventLogMessage (..)
  ) where

import           Database.SQLite.Simple

import qualified Lodjur.Database        as Database
import           Lodjur.Deployment
import           Lodjur.Process

newtype EventLogger = EventLogger Connection

initialize :: Connection -> IO EventLogger
initialize conn = return (EventLogger conn)

data EventLogMessage r where
  -- Public messages:
  AppendEvent :: JobId -> JobEvent -> EventLogMessage Async
  GetEventLogs :: EventLogMessage (Sync EventLogs)

instance Process EventLogger where
  type Message EventLogger = EventLogMessage

  receive _self (a@(EventLogger conn), AppendEvent jobid event) = do
    Database.insertEvent conn (jobEventTime event) jobid event
    return a

  receive _self (a@(EventLogger conn), GetEventLogs) = do
    logs <- Database.getAllEventLogs conn
    return (a, logs)

  terminate (EventLogger conn) = close conn
