{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

module GitHub.Data.CheckRuns where

import GitHub.Data.Apps          (AppRef)
import GitHub.Data.Checks        (Conclusion, CheckStatus)
import GitHub.Data.CheckSuite    (CheckSuite, EventCheckSuite)
import GitHub.Data.Id            (Id)
import GitHub.Data.Name          (Name)
import GitHub.Data.Sha           (Sha)
import GitHub.Data.URL           (URL)
import GitHub.Internal.Prelude
import Prelude ()

data CheckRun = CheckRun
    { checkRunId                :: !(Id CheckRun)
    , checkRunName              :: !(Name CheckRun)
    , checkRunHeadSha           :: !Sha
    , checkRunStatus            :: !CheckStatus
    , checkRunDetailsUrl        :: !(Maybe URL)
    , checkRunExternalId        :: !(Maybe Text)
    , checkRunStartedAt         :: !(Maybe UTCTime)
    , checkRunConclusion        :: !(Maybe Conclusion)
    , checkRunCompletedAt       :: !(Maybe UTCTime)
    , checkRunOutput            :: !(Maybe CheckRunOutput)
    , checkRunCheckSuiteId      :: !(Id CheckSuite)
    }
  deriving (Show, Data, Typeable, Eq, Ord, Generic)

instance NFData CheckRun where rnf = genericRnf
instance Binary CheckRun

instance FromJSON CheckRun where
    parseJSON = withObject "CheckRun" $ \o -> CheckRun
        <$> o .: "id"
        <*> o .: "name"
        <*> o .: "head_sha"
        <*> o .: "status"
        <*> o .:?"details_url"
        <*> o .:?"external_id"
        <*> o .:?"started_at"
        <*> o .:?"conclusion"
        <*> o .:?"completed_at"
        <*> o .:?"output"
        <*> (o .: "check_suite" >>= withObject "CheckSuite" (.: "id"))

data NewCheckRun = NewCheckRun
    { newCheckRunName           :: !(Name CheckRun)
    , newCheckRunHeadSha        :: !Sha
    , newCheckRunDetailsUrl     :: !(Maybe URL)
    , newCheckRunExternalId     :: !(Maybe Text)
    , newCheckRunStatus         :: !(Maybe CheckStatus)
    , newCheckRunStartedAt      :: !(Maybe UTCTime)
    , newCheckRunConclusion     :: !(Maybe Conclusion)
    , newCheckRunCompletedAt    :: !(Maybe UTCTime)
    , newCheckRunOutput         :: !(Maybe CheckRunOutput)
    , newCheckRunActions        :: !(Maybe [CheckRunAction])
    }
  deriving (Show, Data, Typeable, Eq, Ord, Generic)

instance NFData NewCheckRun where rnf = genericRnf
instance Binary NewCheckRun

instance ToJSON NewCheckRun where
    toJSON NewCheckRun {..} = object $ filter notNull
        [ "name"            .= newCheckRunName
        , "head_sha"        .= newCheckRunHeadSha
        , "details_url"     .= newCheckRunDetailsUrl
        , "external_id"     .= newCheckRunExternalId
        , "status"          .= newCheckRunStatus
        , "started_at"      .= newCheckRunStartedAt
        , "conclusion"      .= newCheckRunConclusion
        , "completed_at"    .= newCheckRunCompletedAt
        , "output"          .= newCheckRunOutput
        , "actions"         .= newCheckRunActions
        ]
      where
        notNull (_, Null) = False
        notNull (_, _)    = True

newCheckRun :: Name CheckRun -> Sha -> NewCheckRun
newCheckRun name sha = NewCheckRun
    { newCheckRunName           = name
    , newCheckRunHeadSha        = sha
    , newCheckRunDetailsUrl     = Nothing
    , newCheckRunExternalId     = Nothing
    , newCheckRunStatus         = Nothing
    , newCheckRunStartedAt      = Nothing
    , newCheckRunConclusion     = Nothing
    , newCheckRunCompletedAt    = Nothing
    , newCheckRunOutput         = Nothing
    , newCheckRunActions        = Nothing
    }

data UpdateCheckRun = UpdateCheckRun
    { updateCheckRunName        :: !(Maybe (Name CheckRun))
    , updateCheckRunDetailsUrl  :: !(Maybe URL)
    , updateCheckRunExternalId  :: !(Maybe Text)
    , updateCheckRunStatus      :: !(Maybe CheckStatus)
    , updateCheckRunStartedAt   :: !(Maybe UTCTime)
    , updateCheckRunConclusion  :: !(Maybe Conclusion)
    , updateCheckRunCompletedAt :: !(Maybe UTCTime)
    , updateCheckRunOutput      :: !(Maybe CheckRunOutput)
    , updateCheckRunActions     :: !(Maybe [CheckRunAction])
    }
  deriving (Show, Data, Typeable, Eq, Ord, Generic)

instance NFData UpdateCheckRun where rnf = genericRnf
instance Binary UpdateCheckRun

instance ToJSON UpdateCheckRun where
    toJSON UpdateCheckRun {..} = object $ filter notNull
        [ "name"            .= updateCheckRunName
        , "details_url"     .= updateCheckRunDetailsUrl
        , "external_id"     .= updateCheckRunExternalId
        , "status"          .= updateCheckRunStatus
        , "started_at"      .= updateCheckRunStartedAt
        , "conclusion"      .= updateCheckRunConclusion
        , "completed_at"    .= updateCheckRunCompletedAt
        , "output"          .= updateCheckRunOutput
        , "actions"         .= updateCheckRunActions
        ]
      where
        notNull (_, Null) = False
        notNull (_, _)    = True

emptyUpdateCheckRun :: UpdateCheckRun
emptyUpdateCheckRun = UpdateCheckRun
    { updateCheckRunName        = Nothing
    , updateCheckRunDetailsUrl  = Nothing
    , updateCheckRunExternalId  = Nothing
    , updateCheckRunStatus      = Nothing
    , updateCheckRunStartedAt   = Nothing
    , updateCheckRunConclusion  = Nothing
    , updateCheckRunCompletedAt = Nothing
    , updateCheckRunOutput      = Nothing
    , updateCheckRunActions     = Nothing
    }

data CheckRunOutput = CheckRunOutput
    { checkRunOutputTitle   :: !Text
    , checkRunOutputSummary :: !Text
    , checkRunOutputText    :: !(Maybe Text)
    -- , checkRunAnnotations       :: !(Maybe [CheckRunAnnotation])
    -- , checkRunImages            :: !(Maybe [CheckRunImages])
    }
  deriving (Show, Data, Typeable, Eq, Ord, Generic)

instance NFData CheckRunOutput where rnf = genericRnf
instance Binary CheckRunOutput

instance ToJSON CheckRunOutput where
    toJSON CheckRunOutput {..} = object
        [ "title"           .= checkRunOutputTitle
        , "summary"         .= checkRunOutputSummary
        , "text"            .= checkRunOutputText
        ]

instance FromJSON CheckRunOutput where
    parseJSON = withObject "CheckRunOutput" $ \o -> CheckRunOutput
        <$> o .:? "title"   .!= ""
        <*> o .:? "summary" .!= ""
        <*> o .:? "text"

data CheckRunAction = CheckRunAction
    { checkRunActionLabel       :: !Text
    , checkRunActionDescription :: !Text
    , checkRunActionIdentifier  :: !Text
    }
  deriving (Show, Data, Typeable, Eq, Ord, Generic)

instance NFData CheckRunAction where rnf = genericRnf
instance Binary CheckRunAction

instance ToJSON CheckRunAction where
    toJSON CheckRunAction {..} = object $ filter notNull
        [ "label"           .= checkRunActionLabel
        , "description"     .= checkRunActionDescription
        , "identifier"      .= checkRunActionIdentifier
        ]
      where
        notNull (_, Null) = False
        notNull (_, _)    = True

data EventCheckRun = EventCheckRun
    { eventCheckRunId               :: !(Id CheckRun)
    , eventCheckRunName             :: !(Name CheckRun)
    , eventCheckRunApp              :: !AppRef
    , eventCheckRunCheckSuite       :: !EventCheckSuite
    , eventCheckRunHeadSha          :: !Sha
    , eventCheckRunStatus           :: !CheckStatus
    , eventCheckRunDetailsUrl       :: !(Maybe URL)
    , eventCheckRunExternalId       :: !(Maybe Text)
    , eventCheckRunStartedAt        :: !(Maybe UTCTime)
    , eventCheckRunConclusion       :: !(Maybe Conclusion)
    , eventCheckRunCompletedAt      :: !(Maybe UTCTime)
    , eventCheckRunOutput           :: !(Maybe CheckRunOutput)
    }
  deriving (Show, Data, Typeable, Eq, Ord, Generic)

instance NFData EventCheckRun where rnf = genericRnf
instance Binary EventCheckRun

instance FromJSON EventCheckRun where
    parseJSON = withObject "EventCheckRun" $ \o -> EventCheckRun
        <$> o .: "id"
        <*> o .: "name"
        <*> o .: "app"
        <*> o .: "check_suite"
        <*> o .: "head_sha"
        <*> o .: "status"
        <*> o .:?"details_url"
        <*> o .:?"external_id"
        <*> o .:?"started_at"
        <*> o .:?"conclusion"
        <*> o .:?"completed_at"
        <*> o .:?"output"
