{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}

module Lodjur.Messages where

import           Data.Aeson
import           Data.Text                     (Text)
import           GHC.Generics
import           GitHub.Data.Sha

import           Database.Redis.Queue          (Queue(..))

data WorkerMsg
  = CheckRequested
    { repo              :: !Repo
    , headSha           :: !Sha
    , checkSuiteId      :: !Int
    }
  | RunCheck
    { repo              :: !Repo
    , headSha           :: !Sha
    , checkSuiteId      :: !Int
    , checkRunName      :: !Text
    , checkRunId        :: !Int
    }
  deriving (Show, Eq, Ord, Generic)

instance ToJSON WorkerMsg where
  toJSON = genericToJSON jsonOptions

instance FromJSON WorkerMsg where
  parseJSON = genericParseJSON jsonOptions

data LodjurMsg
  = CreateCheckRun
    { repo              :: !Repo
    , headSha           :: !Sha
    , checkSuiteId      :: !Int
    , checkRunName      :: !Text
    }
  | CheckRunCompleted
    { repo              :: !Repo
    , checkRunId        :: !Int
    , conclusion        :: !Conclusion
    }
  deriving (Show, Eq, Ord, Generic)

instance ToJSON LodjurMsg where
  toJSON = genericToJSON jsonOptions

instance FromJSON LodjurMsg where
  parseJSON = genericParseJSON jsonOptions

data Repo
  = Repo
  { owner   :: !Text
  , name    :: !Text
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
    , fieldLabelModifier = camelTo2 '_'
    }

workersQueue :: Queue
workersQueue = Queue "workers"

lodjurQueue :: Queue
lodjurQueue = Queue "lodjur"