{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE TypeFamilies               #-}
module Lodjur.EventLogger
  ( JobEvent (..)
  , EventLogs
  , EventLog
  , EventLogger
  , emptyEventLogger
  , EventLogMessage (..)
  ) where

import           Data.Semigroup
import           Data.Time.Clock
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap

import           Lodjur.Deployment
import           Lodjur.Process

data JobEvent
  = JobRunning UTCTime
  | JobFinished JobResult UTCTime
  deriving (Show, Eq)

type EventLogs = HashMap JobId EventLog

type EventLog = [JobEvent]

newtype EventLogger = EventLogger EventLogs

emptyEventLogger :: EventLogger
emptyEventLogger = EventLogger mempty

data EventLogMessage r where
  -- Public messages:
  GetEventLogs :: EventLogMessage (Sync EventLogs)
  AppendEvent :: JobId -> JobEvent -> EventLogMessage Async

instance Process EventLogger where
  type Message EventLogger = EventLogMessage

  receive _self (a@(EventLogger logs), GetEventLogs) =
    return (a, logs)

  receive _self (EventLogger logs, AppendEvent jobid event) = do
    putStrLn ("Recording event: " <> show event)
    let events = HashMap.lookupDefault [] jobid logs
    return $ EventLogger (HashMap.insert jobid (event : events) logs)

  terminate (EventLogger _) = return ()
