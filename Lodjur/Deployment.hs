{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Lodjur.Deployment where

import           Data.Aeson
import           Data.Text           (Text)
import           Data.String
import           GHC.Generics        (Generic)
import           Data.Hashable       (Hashable)

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
  } deriving (Show, Eq)

data JobResult
  = JobSuccessful
  | JobFailed Text
  deriving (Show, Eq, Generic)

instance ToJSON JobResult
instance FromJSON JobResult
