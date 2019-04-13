{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Lodjur.Work where

import           Data.Aeson (FromJSON(..), ToJSON(..), genericParseJSON, genericToEncoding)
import           Data.Text                      ( Text )
import           GHC.Generics
import qualified GitHub                        as GH
import qualified GitHub.Extra                  as GH

import           Lodjur.Database.Enum
import           Lodjur.GitHub
import qualified Lodjur.Internal.JSON          as JSON

data Work = Work
  { name            :: !(GH.Name GH.CheckRun)
  , githubSource    :: !Source
  , action          :: !Action
  }
  deriving (Show, Eq, Ord, Generic)

instance ToJSON Work where
  toEncoding = genericToEncoding JSON.options

instance FromJSON Work where
  parseJSON = genericParseJSON JSON.options

data Result = Result
  { conclusion      :: !Conclusion
  , output          :: ![Text]
  , dependencies    :: ![Work]
  }
  deriving (Show, Eq, Ord, Generic)

instance ToJSON Result where
  toEncoding = genericToEncoding JSON.options

instance FromJSON Result where
  parseJSON = genericParseJSON JSON.options

data Action
  = Build
  | Check
  | DeployTo Text
  deriving (Show, Eq, Ord, Generic)

instance ToJSON Action where
  toEncoding = genericToEncoding JSON.options

instance FromJSON Action where
  parseJSON = genericParseJSON JSON.options

data Status = Queued | InProgress | Completed
  deriving (Show, Eq, Ord, Generic)

instance ToJSON Status where
  toEncoding = genericToEncoding JSON.options

instance FromJSON Status where
  parseJSON = genericParseJSON JSON.options

instance TextEnum Status where
  enumToText Queued     = "queued"
  enumToText InProgress = "in_progress"
  enumToText Completed  = "completed"

  enumFromText "queued"      = Just Queued
  enumFromText "in_progress" = Just InProgress
  enumFromText "completed"   = Just Completed
  enumFromText _             = Nothing

data Conclusion = Success | Failure | Neutral | Cancelled
  deriving (Show, Eq, Ord, Generic)

instance ToJSON Conclusion where
  toEncoding = genericToEncoding JSON.options

instance FromJSON Conclusion where
  parseJSON = genericParseJSON JSON.options

instance TextEnum Conclusion where
  enumToText Success    = "success"
  enumToText Failure    = "failure"
  enumToText Neutral    = "neutral"
  enumToText Cancelled  = "cancelled"

  enumFromText "success"     = Just Success
  enumFromText "failure"     = Just Failure
  enumFromText "neutral"     = Just Neutral
  enumFromText "cancelled"   = Just Cancelled
  enumFromText _             = Nothing
