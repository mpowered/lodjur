{-# LANGUAGE DeriveGeneric #-}

module Lodjur.Jobs where

import           Data.Aeson
import           Data.Char                     (isLower)
import           Data.Text                     (Text)
import           GHC.Generics
import           GitHub.Data.Sha

data Job
  = CheckRequested
    { jobRepo           :: !Repo
    , jobHeadSha        :: !Sha
    , jobSuiteId        :: !Int
    }
  | CreateCheckRun
    { jobRepo           :: !Repo
    , jobHeadSha        :: !Sha
    , jobSuiteId        :: !Int
    , jobName           :: !Text
    }
  | CheckRun
    { jobRepo           :: !Repo
    , jobHeadSha        :: !Sha
    , jobSuiteId        :: !Int
    , jobName           :: !Text
    , jobRunId          :: !Int
    }
  | CompleteCheckRun
    { jobRepo           :: !Repo
    , jobRunId          :: !Int
    , jobConclusion     :: !Conclusion
    }
  deriving (Show, Eq, Ord, Generic)

instance ToJSON Job where
  toJSON = genericToJSON jsonOptions

instance FromJSON Job where
  parseJSON = genericParseJSON jsonOptions

data Repo
  = Repo
  { repoOwner       :: !Text
  , repoName        :: !Text
  } deriving (Show, Eq, Ord, Generic)

instance ToJSON Repo where
  toJSON = genericToJSON jsonOptions

instance FromJSON Repo where
  parseJSON = genericParseJSON jsonOptions

data Conclusion = Cancelled | TimedOut | Failed | Neutral | Success
  deriving (Show, Eq, Ord, Generic)

instance ToJSON Conclusion where
  toJSON = genericToJSON jsonOptions

instance FromJSON Conclusion where
  parseJSON = genericParseJSON jsonOptions

data RunStatus = Queued | InProgress | Completed Conclusion
  deriving (Show, Eq, Ord, Generic)

instance ToJSON RunStatus where
  toJSON = genericToJSON jsonOptions

instance FromJSON RunStatus where
  parseJSON = genericParseJSON jsonOptions

jsonOptions :: Options
jsonOptions =
  defaultOptions
    { constructorTagModifier = camelTo2 '_'
    , fieldLabelModifier = camelTo2 '_' . dropPrefix
    }
  where
    dropPrefix = dropWhile isLower
