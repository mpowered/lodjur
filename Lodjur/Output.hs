module Lodjur.Output where

import           Data.HashMap.Strict (HashMap)
import           Data.Time.Clock     (UTCTime)

import Lodjur.Deployment

data Output = Output { outputTime :: UTCTime, outputLines :: [String] }

type OutputLogs = HashMap JobId [Output]
