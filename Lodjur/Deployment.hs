{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
module Lodjur.Deployment where

import           Data.Aeson
import           Data.Hashable   (Hashable)
import           Data.String
import           Data.Text       (Text)
import           Data.Time.Clock (UTCTime)
import           GHC.Generics    (Generic)

import           Lodjur.Git
import           Lodjur.User

newtype DeploymentName =
  DeploymentName { unDeploymentName :: Text }
  deriving (Eq, Show, Generic, Hashable, FromJSON)

instance IsString DeploymentName where
  fromString = DeploymentName . fromString

data Deployment =
  Deployment { deploymentName :: DeploymentName
             , deploymentWarn :: Bool
             }
  deriving (Eq, Show, Generic)

type JobId = Text
type LogType = Text

data DeploymentJob = DeploymentJob
  { jobId                  :: JobId
  , deploymentJobName      :: DeploymentName
  , deploymentRevision     :: Revision
  , deploymentTime         :: UTCTime
  , deploymentType         :: DeploymentType
  , deploymentJobStartedBy :: UserId
  } deriving (Show, Eq)

data DeploymentType
  = BuildDeploy
  | BuildOnly
  | BuildCheck
  deriving (Eq, Ord, Enum, Show)

data JobResult
  = JobSuccessful
  | JobFailed Text
  deriving (Show, Eq, Generic)

instance ToJSON JobResult
instance FromJSON JobResult
