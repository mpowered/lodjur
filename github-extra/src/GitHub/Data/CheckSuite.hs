{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}

module GitHub.Data.CheckSuite where

import GitHub.Data.Apps          (AppRef)
import GitHub.Data.Id            (Id)
import GitHub.Data.Sha           (Sha)
import GitHub.Internal.Prelude
import Prelude ()

data CheckSuite = CheckSuite
  deriving (Show, Data, Typeable, Eq, Ord, Generic)

data EventCheckSuite = EventCheckSuite
    { eventCheckSuiteId             :: !(Id CheckSuite)
    , eventCheckSuiteApp            :: !AppRef
    , eventCheckSuiteStatus         :: !Text
    , eventCheckSuiteHeadSha        :: !Sha
    , eventCheckSuiteHeadBranch     :: !Text
    }
  deriving (Show, Data, Typeable, Eq, Ord, Generic)

instance NFData EventCheckSuite where rnf = genericRnf
instance Binary EventCheckSuite

instance FromJSON EventCheckSuite where
    parseJSON = withObject "EventCheckSuite" $ \o -> EventCheckSuite
        <$> o .: "id"
        <*> o .: "app"
        <*> o .: "status"
        <*> o .: "head_sha"
        <*> o .: "head_branch"
