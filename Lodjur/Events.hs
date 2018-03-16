{-# LANGUAGE DeriveGeneric #-}
module Lodjur.Events where

import           Control.Exception   (Exception)
import           Data.Aeson
import           Data.HashMap.Strict (HashMap)
import           Data.Time.Clock     (UTCTime)
import           GHC.Generics        (Generic)

import           Lodjur.Deployment

data JobEvent
  = JobRunning UTCTime
  | JobFinished JobResult UTCTime
  deriving (Show, Eq, Generic)

instance ToJSON JobEvent
instance FromJSON JobEvent

type EventLogs = HashMap JobId EventLog

type EventLog = [JobEvent]

jobEventTime :: JobEvent -> UTCTime
jobEventTime (JobRunning t   ) = t
jobEventTime (JobFinished _ t) = t

newtype EventDecodeFailed = EventDecodeFailed String
  deriving (Eq, Show)

instance Exception EventDecodeFailed
