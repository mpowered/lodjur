{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}

module Lodjur.GitHub.Events
  ( CheckSuiteEvent (..)
  , CheckRunEvent (..)
  , module GitHub.Data.Webhooks.Events
  ) where

import           Control.DeepSeq          (NFData (..))
import           Control.DeepSeq.Generics (genericRnf)
import           Data.Aeson               (FromJSON(..), withObject, (.:))
import           Data.Data                (Data, Typeable)
import           Data.Text                (Text)
import           GHC.Generics             (Generic)

import           GitHub.Data.Webhooks.Events
import           GitHub.Data.Webhooks.Payload
import           Lodjur.GitHub.Payload

data CheckSuiteEvent = CheckSuiteEvent
  { evCheckSuiteAction          :: !Text
  , evCheckSuiteCheckSuite      :: !HookCheckSuite
  , evCheckSuiteRepository      :: !HookRepository
  , evCheckSuiteOrganization    :: !HookOrganization
  , evCheckSuiteSender          :: !HookUser
  } deriving (Eq, Show, Typeable, Data, Generic)

instance EventHasSender CheckSuiteEvent where senderOfEvent = evCheckSuiteSender
instance EventHasRepo CheckSuiteEvent where repoForEvent = evCheckSuiteRepository
instance NFData CheckSuiteEvent where rnf = genericRnf

data CheckRunEvent = CheckRunEvent
  { evCheckRunAction            :: !Text
  , evCheckRunCheckRun          :: !HookCheckRun
  , evCheckRunRepository        :: !HookRepository
  , evCheckRunOrganization      :: !HookOrganization
  , evCheckRunSender            :: !HookUser
  } deriving (Eq, Show, Typeable, Data, Generic)

instance EventHasSender CheckRunEvent where senderOfEvent = evCheckRunSender
instance EventHasRepo CheckRunEvent where repoForEvent = evCheckRunRepository
instance NFData CheckRunEvent where rnf = genericRnf

instance FromJSON CheckSuiteEvent where
  parseJSON = withObject "CheckSuiteEvent" $ \o -> CheckSuiteEvent
    <$> o .: "action"
    <*> o .: "check_suite"
    <*> o .: "repository"
    <*> o .: "organization"
    <*> o .: "sender"

instance FromJSON CheckRunEvent where
  parseJSON = withObject "CheckRunEvent" $ \o -> CheckRunEvent
    <$> o .: "action"
    <*> o .: "check_run"
    <*> o .: "repository"
    <*> o .: "organization"
    <*> o .: "sender"
