module Lodjur.Output where

import           Data.Text           (Text)
import           Data.Time.Clock     (UTCTime)

type LogId = Text
data Output = Output { outputIndex :: Integer, outputTime :: UTCTime, outputLines :: [String] }
