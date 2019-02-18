{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeApplications  #-}
module Lodjur.Deployment where

import           Data.Aeson
import           Data.Aeson.Types
import           Data.Hashable   (Hashable)
import           Data.String
import           Data.Text       (Text)
import           Data.Time.Clock (UTCTime)
import           Database.PostgreSQL.Simple.FromField
import           Database.PostgreSQL.Simple.ToField
import           GHC.Generics    (Generic)

import           Lodjur.Git
import           Lodjur.User

newtype Target =
  Target { unTarget :: Text }
  deriving (Eq, Show, Generic, Hashable, FromJSON)

instance IsString Target where
  fromString = Target . fromString

data Deployment =
  Deployment { deploymentTarget :: Target
             , deploymentWarn   :: Bool
             }
  deriving (Eq, Show, Generic)

type JobId = Text
type RevisionId = Text

data Job
  = Job
    { jobId             :: JobId
    , jobRevisionId     :: RevisionId
    , jobStartTime      :: UTCTime
    , jobFinishTime     :: UTCTime
    , jobStatus         :: JobStatus
    , jobStartedBy      :: UserId
    }
  deriving (Show, Eq)

data Build
  = Build
    { buildJobId          :: JobId
    }
  deriving (Show, Eq)

data Deploy
  = Deploy
    { deployJobId         :: JobId
    , deployTarget        :: Target
    }
  deriving (Show, Eq)

data Check
  = Check
    { checkJobId          :: JobId
    }
  deriving (Show, Eq)

data JobStatus
  = JobRunning
  | JobSuccessful
  | JobFailed
  deriving (Show, Eq, Generic)

instance ToJSON JobStatus
instance FromJSON JobStatus

instance FromField JobStatus where
  fromField f mdata = do
    txt <- fromField @Text f mdata
    case txt of
      "running" -> return JobRunning
      "successful" -> return JobSuccessful
      "failed" -> return JobFailed

instance ToField JobStatus where
  toField JobRunning = toField @Text "running"
  toField JobSuccessful = toField @Text "successful"
  toField JobFailed = toField @Text "failed"

type AppName = Text

data RSpecResult = RSpecResult
  { rspecExamples       :: [TestResult]
  , rspecSummary        :: RSpecSummary
  } deriving (Show, Eq)

instance FromJSON RSpecResult where
  parseJSON (Object o) = do
    rspecExamples       <- o .: "examples"
    rspecSummary        <- o .: "summary"
    return RSpecResult {..}
  parseJSON invalid = typeMismatch "RSpecResult" invalid

instance ToJSON RSpecResult where
  toJSON RSpecResult {..} = object
    [ "examples" .= rspecExamples
    , "summary"  .= rspecSummary
    ]

instance Semigroup RSpecResult where
  a <> b =
    RSpecResult
      (rspecExamples a <> rspecExamples b)
      (rspecSummary a <> rspecSummary b)

instance Monoid RSpecResult where
  mempty = RSpecResult mempty mempty
  mappend = (<>)

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

instance ToJSON RSpecSummary where
  toJSON RSpecSummary {..} = object
    [ "duration"      .= rspecDuration
    , "example_count" .= rspecExampleCount
    , "failure_count" .= rspecFailureCount
    , "pending_count" .= rspecPendingCount
    ]

instance Semigroup RSpecSummary where
  a <> b =
    RSpecSummary
      (rspecDuration a + rspecDuration b)
      (rspecExampleCount a + rspecExampleCount b)
      (rspecFailureCount a + rspecFailureCount b)
      (rspecPendingCount a + rspecPendingCount b)

instance Monoid RSpecSummary where
  mempty = RSpecSummary 0 0 0 0
  mappend = (<>)

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

instance ToJSON TestResult where
  toJSON TestResult {..} = object
    [ "description"      .= testDescription
    , "full_description" .= testFullDescription
    , "status"           .= testStatus
    , "file_path"        .= testFilePath
    , "line_number"      .= testLineNumber
    , "exception"        .= testException
    ]
