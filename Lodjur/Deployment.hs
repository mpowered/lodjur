{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Lodjur.Deployment where

import           Control.Exception   (Exception)
import           Data.Aeson
import           Data.Hashable       (Hashable)
import           Data.HashMap.Strict (HashMap)
import           Data.String
import           Data.Text           (Text)
import           Data.Time.Clock     (UTCTime)
import           GHC.Generics        (Generic)

newtype Tag =
  Tag { unTag :: Text }
  deriving (Eq, Show, IsString)

newtype DeploymentName =
  DeploymentName { unDeploymentName :: String }
  deriving (Eq, Show, IsString, Generic, Hashable)

type JobId = Text

data DeploymentJob = DeploymentJob
  { jobId          :: JobId
  , deploymentName :: DeploymentName
  , deploymentTag  :: Tag
  , deploymentTime :: UTCTime
  } deriving (Show, Eq)

data JobResult
  = JobSuccessful
  | JobFailed Text
  deriving (Show, Eq, Generic)

instance ToJSON JobResult
instance FromJSON JobResult

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

data Output = Output { outputTime :: UTCTime, outputLines :: [String] }

type OutputLogs = HashMap JobId [Output]

newtype EventDecodeFailed = EventDecodeFailed String
  deriving (Eq, Show)

instance Exception EventDecodeFailed

