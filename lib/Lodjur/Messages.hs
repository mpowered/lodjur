{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}

module Lodjur.Messages where

import           Data.Aeson
import           GHC.Generics
import           GitHub
import           GitHub.Extra

import           Database.Redis.Queue          (Queue(..))

data WorkerMsg
  = CheckRequested
    { repo              :: !RepoRef
    , headSha           :: !Sha
    , checkSuiteId      :: !(Id CheckSuite)
    }
  | RunCheck
    { repo              :: !RepoRef
    , headSha           :: !Sha
    , checkSuiteId      :: !(Id CheckSuite)
    , checkRunName      :: !(Name CheckRun)
    , checkRunId        :: !(Id CheckRun)
    }
  deriving (Show, Eq, Ord, Generic)

instance ToJSON WorkerMsg where
  toJSON = genericToJSON jsonOptions

instance FromJSON WorkerMsg where
  parseJSON = genericParseJSON jsonOptions

data LodjurMsg
  = CreateCheckRun
    { repo              :: !RepoRef
    , headSha           :: !Sha
    , checkSuiteId      :: !(Id CheckSuite)
    , checkRunName      :: !(Name CheckRun)
    }
  | CheckRunInProgress
    { repo              :: !RepoRef
    , checkRunId        :: !(Id CheckRun)
    }
  | CheckRunCompleted
    { repo              :: !RepoRef
    , checkRunId        :: !(Id CheckRun)
    , conclusion        :: !Conclusion
    }
  deriving (Show, Eq, Ord, Generic)

instance ToJSON LodjurMsg where
  toJSON = genericToJSON jsonOptions

instance FromJSON LodjurMsg where
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
