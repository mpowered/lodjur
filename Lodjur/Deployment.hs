{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Lodjur.Deployment where

import           Data.Aeson
import           Data.Hashable   (Hashable)
import           Data.String
import           Data.Text       (Text)
import           Data.Time.Clock (UTCTime)
import           GHC.Generics    (Generic)

import           Lodjur.Git

newtype DeploymentName =
  DeploymentName { unDeploymentName :: String }
  deriving (Eq, Show, IsString, Generic, Hashable)

type JobId = Text

data DeploymentJob = DeploymentJob
  { jobId              :: JobId
  , deploymentName     :: DeploymentName
  , deploymentRevision :: Revision
  , deploymentTime     :: UTCTime
  } deriving (Show, Eq)

data JobResult
  = JobSuccessful
  | JobFailed Text
  deriving (Show, Eq, Generic)

instance ToJSON JobResult
instance FromJSON JobResult
