{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE ViewPatterns               #-}
module Lodjur.EventLogger
  ( JobEvent (..)
  , EventLogs
  , EventLog
  , EventLogger
  , newEventLogger
  , EventLogMessage (..)
  ) where

import           Control.Exception      (Exception, throwIO)
import           Control.Monad
import           Data.Aeson
import           Data.Time.Clock
import           Data.HashMap.Strict    (HashMap)
import qualified Data.HashMap.Strict    as HashMap
import           Database.SQLite.Simple
import           GHC.Generics           (Generic)

import           Lodjur.Deployment
import           Lodjur.Process

newtype EventLogger = EventLogger Connection

data JobEvent
  = JobRunning UTCTime
  | JobFinished JobResult UTCTime
  deriving (Show, Eq, Generic)

instance ToJSON JobEvent
instance FromJSON JobEvent

jobEventTime :: JobEvent -> UTCTime
jobEventTime (JobRunning t) = t
jobEventTime (JobFinished _ t) = t

type EventLogs = HashMap JobId EventLog

type EventLog = [JobEvent]

newtype EventDecodeFailed = EventDecodeFailed String
  deriving (Eq, Show)

instance Exception EventDecodeFailed

newEventLogger :: String -> IO EventLogger
newEventLogger file = EventLogger <$> openAndInit file

openAndInit :: String -> IO Connection
openAndInit file = do
    conn <- open file
    execute_ conn "CREATE TABLE IF NOT EXISTS job_event_log (time TEXT, job_id TEXT, event TEXT)"
    return conn

insertEvent :: ToJSON event => Connection -> UTCTime -> JobId -> event -> IO ()
insertEvent conn t jobid event =
  execute conn "INSERT INTO job_event_log (time, job_id, event) VALUES (?, ?, ?)"
    (t, jobid, encode event)

getAllEventLogs :: Connection -> IO EventLogs
getAllEventLogs conn = mkEventLog =<< query_ conn "SELECT job_id, event FROM job_event_log ORDER BY time ASC"
 where
  mkEventLog = foldM mergeEvent mempty
  mergeEvent m (jobid, eitherDecode -> event) =
    case event of
      Left msg -> throwIO $ EventDecodeFailed msg
      Right e -> return $ HashMap.insertWith (++) jobid [e] m

data EventLogMessage r where
  -- Public messages:
  AppendEvent :: JobId -> JobEvent -> EventLogMessage Async
  GetEventLogs :: EventLogMessage (Sync EventLogs)

instance Process EventLogger where
  type Message EventLogger = EventLogMessage

  receive _self (a@(EventLogger conn), AppendEvent jobid event) = do
    insertEvent conn (jobEventTime event) jobid event
    return a

  receive _self (a@(EventLogger conn), GetEventLogs) = do
    logs <- getAllEventLogs conn
    return (a, logs)

  terminate (EventLogger conn) = close conn
