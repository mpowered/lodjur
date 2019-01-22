module Lodjur.Output where

import           Data.Time.Clock     (UTCTime)

data Output = Output { outputIndex :: Integer, outputTime :: UTCTime, outputLines :: [String] }
