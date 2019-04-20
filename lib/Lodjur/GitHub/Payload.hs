{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}

module Lodjur.GitHub.Payload
  ( CheckStatus
  , CheckConclusion
  , HookCheckSuite (..)
  , HookCheckRun (..)
  , HookApp (..)
  , HookAnnotation (..)
  , HookOutput (..)
  , module GitHub.Data.Webhooks.Payload
  ) where

import           GitHub.Data.Webhooks.Payload

import           Control.DeepSeq          (NFData (..))
import           Control.DeepSeq.Generics (genericRnf)
import           Data.Aeson               (FromJSON(..), withObject, (.:), (.:?), (.!=))
import           Data.Data                (Data, Typeable)
import           Data.Text                (Text)
import           Data.Time                (UTCTime)
import           GHC.Generics             (Generic)

type CheckStatus = Text

type CheckConclusion = Text

data HookCheckSuite = HookCheckSuite
  { whSuiteId                   :: !Int
  , whSuiteHeadBranch           :: !Text
  , whSuiteHeadSha              :: !Text
  , whSuiteHeadCommit           :: !HookCommit
  , whSuiteStatus               :: !CheckStatus
  , whSuiteConclusion           :: !(Maybe CheckConclusion)
  , whSuiteUrl                  :: !URL
  , whSuiteApp                  :: !HookApp
  } deriving (Eq, Show, Typeable, Data, Generic)

instance NFData HookCheckSuite where rnf = genericRnf

data HookCheckRun = HookCheckRun
  { whRunId                     :: !Int
  , whRunName                   :: !Text
  , whRunApp                    :: !HookApp
  , whRunCheckSuite             :: !HookCheckSuite
  , whRunHeadSha                :: !Text
  , whRunStatus                 :: !CheckStatus
  , whRunDetailsUrl             :: !(Maybe URL)
  , whRunExternalId             :: !(Maybe Text)
  , whRunStartedAt              :: !(Maybe UTCTime)
  , whRunConclusion             :: !(Maybe CheckConclusion)
  , whRunCompletedAt            :: !(Maybe UTCTime)
  , whRunOutput                 :: !(Maybe HookOutput)
  } deriving (Eq, Show, Typeable, Data, Generic)

instance NFData HookCheckRun where rnf = genericRnf

data HookApp = HookApp
  { whAppId                     :: !Int
  , whAppOwner                  :: !(Either HookSimpleUser HookUser)
  , whAppName                   :: !Text
  , whAppDescription            :: !(Maybe Text)
  , whAppExternalUrl            :: !(Maybe URL)
  , whAppHtmlUrl                :: !(Maybe URL)
  , whAppCreatedAt              :: !(Maybe UTCTime)
  , whAppUpdatedAt              :: !(Maybe UTCTime)
  } deriving (Eq, Show, Typeable, Data, Generic)

instance NFData HookApp where rnf = genericRnf

data HookOutput = HookOutput
  { whOutputTitle               :: !Text
  , whOutputSummary             :: !Text
  , whOutputText                :: !(Maybe Text)
  , whOutputAnnotations         :: ![HookAnnotation]
  } deriving (Eq, Show, Typeable, Data, Generic)

instance NFData HookOutput where rnf = genericRnf

data HookAnnotation = HookAnnotation
  { whAnnotationPath            :: !Text
  , whAnnotationStartLine       :: !Integer
  , whAnnotationEndLine         :: !Integer
  , whAnnotationStartColumn     :: !(Maybe Integer)
  , whAnnotationEndColumn       :: !(Maybe Integer)
  , whAnnotationLevel           :: !Text
  , whAnnotationMessage         :: !Text
  , whAnnotationTitle           :: !(Maybe Text)
  , whAnnotationRawDetails      :: !(Maybe Text)
  } deriving (Eq, Show, Typeable, Data, Generic)

instance NFData HookAnnotation where rnf = genericRnf

instance FromJSON HookCheckSuite where
  parseJSON = withObject "HookCheckSuite" $ \o -> HookCheckSuite
    <$> o .:  "id"
    <*> o .:  "head_branch"
    <*> o .:  "head_sha"
    <*> o .:  "status"
    <*> o .:  "conclusion"
    <*> o .:  "url"
    <*> o .:  "app"
    <*> o .:  "repository"

instance FromJSON HookCheckRun where
  parseJSON = withObject "HookCheckRun" $ \o -> HookCheckRun
    <$> o .:  "id"
    <*> o .:  "name"
    <*> o .:  "app"
    <*> o .:  "check_suite"
    <*> o .:  "head_sha"
    <*> o .:  "status"
    <*> o .:? "details_url"
    <*> o .:? "external_id"
    <*> o .:? "started_at"
    <*> o .:? "conclusion"
    <*> o .:? "completed_at"
    <*> o .:? "output"

instance FromJSON HookApp where
  parseJSON = withObject "HookApp" $ \o -> HookApp
    <$> o .:  "id"
    <*> o .:  "owner"
    <*> o .:  "name"
    <*> o .:? "description"
    <*> o .:? "external_url"
    <*> o .:? "html_url"
    <*> o .:? "created_at"
    <*> o .:? "updated_at"

instance FromJSON HookOutput where
  parseJSON = withObject "HookOutput" $ \o -> HookOutput
    <$> o .:? "title"       .!= ""
    <*> o .:? "summary"     .!= ""
    <*> o .:? "text"
    <*> o .:? "annotations" .!= []

instance FromJSON HookAnnotation where
  parseJSON = withObject "HookAnnotation" $ \o -> HookAnnotation
    <$> o .:  "path"
    <*> o .:  "start_line"
    <*> o .:  "end_line"
    <*> o .:? "start_column"
    <*> o .:? "end_column"
    <*> o .:  "annotation_level"
    <*> o .:  "message"
    <*> o .:? "title"
    <*> o .:? "raw_details"
