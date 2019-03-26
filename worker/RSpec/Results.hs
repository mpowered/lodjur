{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE OverloadedStrings #-}

module RSpec.Results where

import           Data.Aeson
import           Data.Text

data RSpecResult = RSpecResult
  { rspecExamples       :: [TestResult]
  , rspecSummary        :: RSpecSummary
  } deriving (Show, Eq)

instance FromJSON RSpecResult where
  parseJSON = withObject "RSpecResult" $ \o -> do
    rspecExamples <- o .: "examples"
    rspecSummary  <- o .: "summary"
    return RSpecResult { .. }

instance ToJSON RSpecResult where
  toJSON RSpecResult {..} =
    object ["examples" .= rspecExamples, "summary" .= rspecSummary]

instance Semigroup RSpecResult where
  a <> b = RSpecResult (rspecExamples a <> rspecExamples b)
                       (rspecSummary a <> rspecSummary b)

instance Monoid RSpecResult where
  mempty  = RSpecResult mempty mempty
  mappend = (<>)

data RSpecSummary = RSpecSummary
  { rspecDuration       :: Float
  , rspecExampleCount   :: Int
  , rspecFailureCount   :: Int
  , rspecPendingCount   :: Int
  } deriving (Show, Eq)

instance FromJSON RSpecSummary where
  parseJSON = withObject "RSpecSummary" $ \o -> do
    rspecDuration     <- o .: "duration"
    rspecExampleCount <- o .: "example_count"
    rspecFailureCount <- o .: "failure_count"
    rspecPendingCount <- o .: "pending_count"
    return RSpecSummary { .. }

instance ToJSON RSpecSummary where
  toJSON RSpecSummary {..} = object
    [ "duration"      .= rspecDuration
    , "example_count" .= rspecExampleCount
    , "failure_count" .= rspecFailureCount
    , "pending_count" .= rspecPendingCount
    ]

instance Semigroup RSpecSummary where
  a <> b = RSpecSummary (rspecDuration a + rspecDuration b)
                        (rspecExampleCount a + rspecExampleCount b)
                        (rspecFailureCount a + rspecFailureCount b)
                        (rspecPendingCount a + rspecPendingCount b)

instance Monoid RSpecSummary where
  mempty  = RSpecSummary 0 0 0 0
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
  parseJSON = withObject "TestResult" $ \o -> do
    testDescription     <- o .: "description"
    testFullDescription <- o .: "full_description"
    testStatus          <- o .: "status"
    testFilePath        <- o .: "file_path"
    testLineNumber      <- o .: "line_number"
    testException       <- o .:? "exception"
    return TestResult { .. }

instance ToJSON TestResult where
  toJSON TestResult {..} = object
    [ "description"      .= testDescription
    , "full_description" .= testFullDescription
    , "status"           .= testStatus
    , "file_path"        .= testFilePath
    , "line_number"      .= testLineNumber
    , "exception"        .= testException
    ]
