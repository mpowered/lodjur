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
    , eventCheckSuiteHeadCommit     :: !(Maybe EventCheckSuiteCommit)
    , eventCheckSuiteStatus         :: !CheckStatus
    , eventCheckSuiteConclusion     :: !(Maybe Conclusion)
    , eventCheckSuiteUrl            :: !URL
    , eventCheckSuiteApp            :: !App
    }
  deriving (Show, Data, Typeable, Eq, Ord, Generic)

instance NFData EventCheckSuite where rnf = genericRnf
instance Binary EventCheckSuite

data EventCheckSuiteCommit = EventCheckSuiteCommit
    { eventCheckSuiteCommitId        :: !Sha
    , eventCheckSuiteCommitTreeId    :: !Sha
    , eventCheckSuiteCommitMessage   :: !Text
    , eventCheckSuiteCommitTimestamp :: !UTCTime
    , eventCheckSuiteCommitAuthor    :: !(Maybe EventCheckSuiteUser)
    , eventCheckSuiteCommitCommitter :: !(Maybe EventCheckSuiteUser)
    }
  deriving (Show, Data, Typeable, Eq, Ord, Generic)

instance NFData EventCheckSuiteCommit where rnf = genericRnf
instance Binary EventCheckSuiteCommit

data EventCheckSuiteUser = EventCheckSuiteUser
    { eventCheckSuiteUserName        :: !Text
    , eventCheckSuiteUserEmail       :: !Text
    }
  deriving (Show, Data, Typeable, Eq, Ord, Generic)

instance NFData EventCheckSuiteUser where rnf = genericRnf
instance Binary EventCheckSuiteUser

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
        <*> o .:?"head_commit"
        <*> o .: "status"
        <*> o .: "conclusion"
        <*> o .: "url"
        <*> o .: "app"

instance FromJSON EventCheckSuiteCommit where
    parseJSON = withObject "EventCheckSuiteCommit" $ \o -> EventCheckSuiteCommit
        <$> o .: "id"
        <*> o .: "tree_id"
        <*> o .: "message"
        <*> o .: "timestamp"
        <*> o .: "author"
        <*> o .: "committer"

instance ToJSON EventCheckSuiteCommit where
    toJSON EventCheckSuiteCommit {..} = object
        [ "id"            .= eventCheckSuiteCommitId
        , "tree_id"       .= eventCheckSuiteCommitTreeId
        , "message"       .= eventCheckSuiteCommitMessage
        , "timestamp"     .= eventCheckSuiteCommitTimestamp
        , "author"        .= eventCheckSuiteCommitAuthor
        , "committer"     .= eventCheckSuiteCommitCommitter
        ]

instance FromJSON EventCheckSuiteUser where
    parseJSON = withObject "EventCheckSuiteUser" $ \o -> EventCheckSuiteUser
        <$> o .: "name"
        <*> o .: "email"

instance ToJSON EventCheckSuiteUser where
    toJSON EventCheckSuiteUser {..} = object
        [ "name"          .= eventCheckSuiteUserName
        , "email"         .= eventCheckSuiteUserEmail
        ]

