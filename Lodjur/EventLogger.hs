{-# LANGUAGE GADTs        #-}
{-# LANGUAGE TypeFamilies #-}
module Lodjur.EventLogger
  ( JobEvent (..)
  , EventLogs
  , EventLog
  , EventLogger
  , initialize
  , EventLogMessage (..)
  ) where

import           Lodjur.Database          (DbPool)
import           Lodjur.Deployment
import qualified Lodjur.EventLogger.Database as Database
import           Lodjur.Process

newtype EventLogger = EventLogger DbPool

initialize :: DbPool -> IO EventLogger
initialize pool = do
  Database.initialize pool
  return (EventLogger pool)

data EventLogMessage r where
  -- Public messages:
  AppendEvent :: JobId -> JobEvent -> EventLogMessage Async
  GetEventLogs :: EventLogMessage (Sync EventLogs)

instance Process EventLogger where
  type Message EventLogger = EventLogMessage

  receive _self (a@(EventLogger pool), AppendEvent jobid event) = do
    Database.insertEvent pool (jobEventTime event) jobid event
    return a

  receive _self (a@(EventLogger pool), GetEventLogs) = do
    logs <- Database.getAllEventLogs pool
    return (a, logs)

  terminate _ = return ()
