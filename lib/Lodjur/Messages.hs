{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}

module Lodjur.Messages where

import           Data.Aeson
import           Data.Text                     (Text)
import           GHC.Generics
import           GitHub
import           GitHub.Extra

import           Database.Redis.Queue          (Queue(..))

data WorkerMsg
  = CreateCheckRun
    { checkSuiteId          :: !(Id CheckSuite)
    , checkRunName          :: !(Name CheckRun)
    , checkRunExternalId    :: !(Maybe Text)
    , checkRunOutput        :: !(Maybe CheckRunOutput)
    }
  | CheckRunInProgress
    { checkRunId            :: !(Id CheckRun)
    , checkRunOutput        :: !(Maybe CheckRunOutput)
    }
  | CheckRunCompleted
    { checkRunId            :: !(Id CheckRun)
    , conclusion            :: !Conclusion
    , checkRunOutput        :: !(Maybe CheckRunOutput)
    }
  deriving (Show, Eq, Ord, Generic)

instance ToJSON WorkerMsg where
  toJSON = genericToJSON jsonOptions

instance FromJSON WorkerMsg where
  parseJSON = genericParseJSON jsonOptions

jsonOptions :: Options
jsonOptions =
  defaultOptions
    { constructorTagModifier = camelTo2 '_'
    , fieldLabelModifier = camelTo2 '_'
    }

checkRequestedQueue :: Queue (Id CheckSuite)
checkRequestedQueue = Queue "checks-requested"

checkInProgressQueue :: Queue (Id CheckSuite)
checkInProgressQueue = Queue "checks-in-progress"

checkCompletedQueue :: Queue (Id CheckSuite)
checkCompletedQueue = Queue "checks-completed"

runRequestedQueue :: Queue (Id CheckRun)
runRequestedQueue = Queue "runs-requested"

runInProgressQueue :: Queue (Id CheckRun)
runInProgressQueue = Queue "runs-in-progress"

workerQueue :: Queue WorkerMsg
workerQueue = Queue "worker-msgs"
