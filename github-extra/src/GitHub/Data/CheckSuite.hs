{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

module GitHub.Data.CheckSuite where

import GitHub.Data.Apps          (App)
import GitHub.Data.Checks        (CheckStatus, Conclusion)
import GitHub.Data.Id            (Id)
import GitHub.Data.Repos         (Repo)
import GitHub.Data.Sha           (Sha)
import GitHub.Data.URL           (URL)
import GitHub.Internal.Prelude
import Prelude ()

data CheckSuite = CheckSuite
    { checkSuiteId                  :: !(Id CheckSuite)
    , checkSuiteHeadBranch          :: !Text
    , checkSuiteHeadSha             :: !Sha
    , checkSuiteStatus              :: !CheckStatus
    , checkSuiteConclusion          :: !(Maybe Conclusion)
    , checkSuiteUrl                 :: !URL
    , checkSuiteApp                 :: !App
    , checkSuiteRepository          :: !Repo
    }
  deriving (Show, Data, Typeable, Eq, Ord, Generic)

instance NFData CheckSuite where rnf = genericRnf
instance Binary CheckSuite

data EventCheckSuite = EventCheckSuite
    { eventCheckSuiteId             :: !(Id CheckSuite)
    , eventCheckSuiteHeadBranch     :: !Text
    , eventCheckSuiteHeadSha        :: !Sha
    , eventCheckSuiteStatus         :: !CheckStatus
    , eventCheckSuiteConclusion     :: !(Maybe Conclusion)
    , eventCheckSuiteUrl            :: !URL
    , eventCheckSuiteApp            :: !App
    }
  deriving (Show, Data, Typeable, Eq, Ord, Generic)

instance NFData EventCheckSuite where rnf = genericRnf
instance Binary EventCheckSuite

instance FromJSON CheckSuite where
    parseJSON = withObject "CheckSuite" $ \o -> CheckSuite
        <$> o .: "id"
        <*> o .: "head_branch"
        <*> o .: "head_sha"
        <*> o .: "status"
        <*> o .: "conclusion"
        <*> o .: "url"
        <*> o .: "app"
        <*> o .: "repository"

instance ToJSON CheckSuite where
    toJSON CheckSuite {..} = object
        [ "id"            .= checkSuiteId
        , "head_branch"   .= checkSuiteHeadBranch
        , "head_sha"      .= checkSuiteHeadSha
        , "status"        .= checkSuiteStatus
        , "conclusion"    .= checkSuiteConclusion
        , "url"           .= checkSuiteUrl
        , "app"           .= checkSuiteApp
        , "repository"    .= checkSuiteRepository
        ]

instance FromJSON EventCheckSuite where
    parseJSON = withObject "EventCheckSuite" $ \o -> EventCheckSuite
        <$> o .: "id"
        <*> o .: "head_branch"
        <*> o .: "head_sha"
        <*> o .: "status"
        <*> o .: "conclusion"
        <*> o .: "url"
        <*> o .: "app"
