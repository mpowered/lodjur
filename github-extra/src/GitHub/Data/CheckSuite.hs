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

data NewCheckSuite = NewCheckSuite
    { newCheckSuiteHeadSha      :: !Sha
    }
  deriving (Show, Data, Typeable, Eq, Ord, Generic)

instance NFData NewCheckSuite where rnf = genericRnf
instance Binary NewCheckSuite

instance ToJSON NewCheckSuite where
    toJSON NewCheckSuite {..} = object
        [ "head_sha"        .= newCheckSuiteHeadSha
        ]

newCheckSuite :: Sha -> NewCheckSuite
newCheckSuite sha = NewCheckSuite
    { newCheckSuiteHeadSha      = sha
    }