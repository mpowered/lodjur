{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TypeApplications   #-}

module GitHub.Data.Checks where

import qualified Data.Text as Text
import GitHub.Internal.Prelude
import Prelude ()

data CheckStatus = Queued | InProgress | Completed
  deriving (Show, Data, Typeable, Eq, Ord, Generic)

instance NFData CheckStatus where rnf = genericRnf
instance Binary CheckStatus

instance ToJSON CheckStatus where
  toJSON Queued         = toJSON @Text "queued"
  toJSON InProgress     = toJSON @Text "in_progress"
  toJSON Completed      = toJSON @Text "completed"

instance FromJSON CheckStatus where
  parseJSON = withText "CheckStatus" $ \case
    "queued"          -> return Queued
    "in_progress"     -> return InProgress
    "completed"       -> return Completed
    x                 -> fail $ Text.unpack x ++ " is not a valid CheckStatus"

data Conclusion = Success | Failure | Neutral | Cancelled | TimedOut | ActionRequired
  deriving (Show, Data, Typeable, Eq, Ord, Generic)

instance NFData Conclusion where rnf = genericRnf
instance Binary Conclusion

instance ToJSON Conclusion where
  toJSON Success        = toJSON @Text "success"
  toJSON Failure        = toJSON @Text "failure"
  toJSON Neutral        = toJSON @Text "neutral"
  toJSON Cancelled      = toJSON @Text "cancelled"
  toJSON TimedOut       = toJSON @Text "timed_out"
  toJSON ActionRequired = toJSON @Text "action_required"

instance FromJSON Conclusion where
  parseJSON = withText "Conclusion" $ \case
    "success"         -> return Success
    "failure"         -> return Failure
    "neutral"         -> return Neutral
    "cancelled"       -> return Cancelled
    "timed_out"       -> return TimedOut
    "action_required" -> return ActionRequired
    x                 -> fail $ Text.unpack x ++ " is not a valid Conclusion"
