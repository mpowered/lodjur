{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeFamilies          #-}

module Lodjur.Database.Checks where

import           Data.Aeson
import           Data.HashSet           (HashSet)
import           Data.ByteString        (ByteString)
import qualified Data.ByteString.Char8  as BS
import           Data.Text              (Text)
import           Data.Time.Clock        (UTCTime)
import           GHC.Generics
import           GitHub                 (Id, Name)
import qualified GitHub                 as GH
import           GitHub.Extra           (Sha(..))
import qualified GitHub.Extra           as GH

import           Lodjur.Database.Types

data CheckSuite = CheckSuite
    { headBranch            :: !Text
    , headSha               :: !Sha
    , status                :: !GH.CheckStatus
    , conclusion            :: !(Maybe GH.Conclusion)
    , repository            :: !GH.Repo
    , checkRuns             :: !(HashSet (Id CheckRun))
    }
  deriving (Show, Eq, Ord, Generic)

instance ToJSON CheckSuite where
  toJSON = genericToJSON jsonOptions

instance FromJSON CheckSuite where
  parseJSON = genericParseJSON jsonOptions

instance HasKey CheckSuite where
  data Key CheckSuite = CheckSuiteKey ByteString
  redisKey (CheckSuiteKey bs) = "checksuite:" <> bs

checkSuiteKeyFromId :: Id GH.CheckSuite -> Key CheckSuite
checkSuiteKeyFromId = CheckSuiteKey . BS.pack . show . GH.untagId

data CheckRun = CheckRun
    { checkSuiteId          :: !(Id GH.CheckSuite)
    , name                  :: !(Name GH.CheckRun)
    , headSha               :: !Sha
    , status                :: !GH.CheckStatus
    , conclusion            :: !(Maybe GH.Conclusion)
    , startedAt             :: !(Maybe UTCTime)
    , completedAt           :: !(Maybe UTCTime)
    , output                :: !(Maybe GH.CheckRunOutput)
    }
  deriving (Show, Eq, Ord, Generic)

instance ToJSON CheckRun where
  toJSON = genericToJSON jsonOptions

instance FromJSON CheckRun where
  parseJSON = genericParseJSON jsonOptions

instance HasKey CheckRun where
  data Key CheckRun = CheckRunKey ByteString
  redisKey (CheckRunKey bs) = "checkrun:" <> bs

checkRunKeyFromId :: Id GH.CheckRun -> Key CheckRun
checkRunKeyFromId = CheckRunKey . BS.pack . show . GH.untagId

jsonOptions :: Options
jsonOptions =
  defaultOptions
    { constructorTagModifier = camelTo2 '_'
    , fieldLabelModifier = camelTo2 '_'
    }
