{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

module GitHub.Data.CheckRuns where

import GitHub.Data.Id            (Id)
import GitHub.Data.Name          (Name)
import GitHub.Data.URL           (URL)
import GitHub.Internal.Prelude
import Prelude ()

newtype Sha = Sha { getSha :: Text }
  deriving (Show, Data, Typeable, Eq, Ord, Generic)

instance NFData Sha where rnf = genericRnf
instance Binary Sha
instance Hashable Sha where
    hashWithSalt salt (Sha l) = hashWithSalt salt l
instance IsString Sha where
    fromString = Sha . fromString

instance FromJSON Sha where
    parseJSON = withText "Sha" (pure . Sha)

instance ToJSON Sha where
    toJSON = toJSON . getSha

data CheckRun = CheckRun
    { checkRunId                :: !(Id CheckRun)
    , checkRunHeadSha           :: !Sha
    , checkRunStatus            :: !Text
    , checkRunDetailsUrl        :: !(Maybe URL)
    , checkRunExternalId        :: !(Maybe Text)
    , checkRunStartedAt         :: !(Maybe UTCTime)
    , checkRunConclusion        :: !(Maybe Text)
    , checkRunCompletedAt       :: !(Maybe UTCTime)
    , checkRunOutput            :: !(Maybe CheckRunOutput)
    }
  deriving (Show, Data, Typeable, Eq, Ord, Generic)

instance NFData CheckRun where rnf = genericRnf
instance Binary CheckRun

instance FromJSON CheckRun where
    parseJSON = withObject "CheckRun" $ \o -> CheckRun
        <$> o .: "id"
        <*> o .: "head_sha"
        <*> o .: "status"
        <*> o .:?"details_url"
        <*> o .:?"external_id"
        <*> o .:?"started_at"
        <*> o .:?"conclusion"
        <*> o .:?"completed_at"
        <*> o .:?"output"

data NewCheckRun = NewCheckRun
    { newCheckRunName           :: !(Name CheckRun)
    , newCheckRunHeadSha        :: !Sha
    , newCheckRunDetailsUrl     :: !(Maybe URL)
    , newCheckRunExternalId     :: !(Maybe Text)
    , newCheckRunStatus         :: !(Maybe Text)
    , newCheckRunStartedAt      :: !(Maybe UTCTime)
    , newCheckRunConclusion     :: !(Maybe Text)
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
