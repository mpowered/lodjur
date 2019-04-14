{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE OverloadedStrings #-}

module Lodjur.RSpec where

import           Data.Aeson
import           Data.Text

data RSpecResult = RSpecResult
  { rspecExamples       :: ![TestResult]
  , rspecSummary        :: !RSpecSummary
  } deriving (Show, Eq, Ord)

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
  { rspecDuration       :: !Double
  , rspecExampleCount   :: !Int
  , rspecFailureCount   :: !Int
  , rspecPendingCount   :: !Int
  } deriving (Show, Eq, Ord)

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
  { testDescription     :: !Text
  , testFullDescription :: !Text
  , testStatus          :: !Text
  , testFilePath        :: !Text
  , testLineNumber      :: !Int
  , testException       :: !(Maybe TestException)
  } deriving (Show, Eq, Ord)

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

data TestException = TestException
  { exceptionClass     :: !Text
  , exceptionMessage   :: !Text
  , exceptionBacktrace :: ![Text]
  } deriving (Show, Eq, Ord)

instance FromJSON TestException where
  parseJSON = withObject "TestException" $ \o -> do
    exceptionClass     <- o .: "class"
    exceptionMessage   <- o .: "message"
    exceptionBacktrace <- o .: "backtrace"
    return TestException { .. }

instance ToJSON TestException where
  toJSON TestException {..} = object
    [ "class"     .= exceptionClass
    , "message"   .= exceptionMessage
    , "backtrace" .= exceptionBacktrace
    ]
