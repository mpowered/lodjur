{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Web.WebHook.Events where

import           Data.Aeson
import           Data.Aeson.Types
import           Data.Text
import           GitHub
import           GitHub.Extra

data PushEvent = PushEvent
  { pushRef        :: !Text
  , pushRepository :: !RepoRef
  } deriving (Eq, Show)

instance FromJSON PushEvent where
  parseJSON (Object o) = do
    pushRef         <- o .: "ref"
    pushRepository  <- o .: "repository"
    return PushEvent {..}
  parseJSON invalid = typeMismatch "PushEvent" invalid

data CreateEvent = CreateEvent
  { createRef        :: !Text
  , createRepository :: !Repo
  } deriving (Eq, Show)

instance FromJSON CreateEvent where
  parseJSON (Object o) = do
    createRef        <- o .: "ref"
    createRepository <- o .: "repository"
    return CreateEvent {..}
  parseJSON invalid = typeMismatch "CreateEvent" invalid

data DeleteEvent = DeleteEvent
  { deleteRef        :: !Text
  , deleteRepository :: !Repo
  } deriving (Eq, Show)

instance FromJSON DeleteEvent where
  parseJSON (Object o) = do
    deleteRef        <- o .: "ref"
    deleteRepository <- o .: "repository"
    return DeleteEvent {..}
  parseJSON invalid = typeMismatch "DeleteEvent" invalid

data CheckSuiteEvent = CheckSuiteEvent
  { checkSuiteEventAction       :: !Text
  , checkSuiteEventRepository   :: !Repo
  , checkSuiteEventCheckSuite   :: !EventCheckSuite
  } deriving (Eq, Show)

instance FromJSON CheckSuiteEvent where
  parseJSON (Object o) = do
    checkSuiteEventAction       <- o .: "action"
    checkSuiteEventRepository   <- o .: "repository"
    checkSuiteEventCheckSuite   <- o .: "check_suite"
    return CheckSuiteEvent {..}
  parseJSON invalid = typeMismatch "CheckSuiteEvent" invalid

data CheckRunEvent = CheckRunEvent
  { checkRunEventAction       :: !Text
  , checkRunEventCheckRun     :: !EventCheckRun
  , checkRunEventRepository   :: !Repo
  } deriving (Eq, Show)

instance FromJSON CheckRunEvent where
  parseJSON (Object o) = do
    checkRunEventAction       <- o .: "action"
    checkRunEventCheckRun     <- o .: "check_run"
    checkRunEventRepository   <- o .: "repository"
    return CheckRunEvent {..}
  parseJSON invalid = typeMismatch "CheckRunEvent" invalid
