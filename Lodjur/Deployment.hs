{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Lodjur.Deployment where

import           Data.Aeson
import           Data.Aeson.Types
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

type AppName = Text

data RSpecResult = RSpecResult
  { rspecExamples       :: Value        -- [TestResult]
  , rspecSummary        :: RSpecSummary
  } deriving (Show, Eq)

instance FromJSON RSpecResult where
  parseJSON (Object o) = do
    rspecExamples       <- o .: "examples"
    rspecSummary        <- o .: "summary"
    return RSpecResult {..}
  parseJSON invalid = typeMismatch "RSpecResult" invalid

data RSpecSummary = RSpecSummary
  { rspecDuration       :: Float
  , rspecExampleCount   :: Int
  , rspecFailureCount   :: Int
  , rspecPendingCount   :: Int
  } deriving (Show, Eq)

instance FromJSON RSpecSummary where
  parseJSON (Object o) = do
    rspecDuration       <- o .: "duration"
    rspecExampleCount   <- o .: "example_count"
    rspecFailureCount   <- o .: "failure_count"
    rspecPendingCount   <- o .: "pending_count"
    return RSpecSummary {..}
  parseJSON invalid = typeMismatch "RSpecSummary" invalid

data TestResult = TestResult
  { testDescription     :: Text
  , testFullDescription :: Text
  , testStatus          :: Text
  , testFilePath        :: Text
  , testLineNumber      :: Integer
  , testException       :: Maybe Value
  } deriving (Show, Eq)

instance FromJSON TestResult where
  parseJSON (Object o) = do
    testDescription     <- o .: "description"
    testFullDescription <- o .: "full_description"
    testStatus          <- o .: "status"
    testFilePath        <- o .: "file_path"
    testLineNumber      <- o .: "line_number"
    testException       <- o .:? "exception"
    return TestResult {..}
  parseJSON invalid = typeMismatch "TestResult" invalid

data CheckResult = CheckResult
  { checkPassed         :: Int
  , checkFailed         :: Int
  , checkDuration       :: Float
  , checkTests          :: [TestResult]
  } deriving (Show, Eq)

instance Semigroup CheckResult where
  a <> b =
    CheckResult
      (checkPassed a + checkPassed b)
      (checkFailed a + checkFailed b)
      (checkDuration a + checkDuration b)
      (checkTests a <> checkTests b)
