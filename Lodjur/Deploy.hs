{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeFamilies               #-}
module Lodjur.Deploy
  ( Tag (..)
  , DeploymentName (..)
  , JobId
  , DeploymentJob (..)
  , DeployState (..)
  , JobEvent (..)
  , JobResult (..)
  , EventLogs
  , EventLog
  , Deployer
  , DeployMessage (..)
  , EventLogger (..)
  , EventLogMessage (..)
  , initialize
  ) where

import           Control.Concurrent
import           Control.Exception   (Exception, SomeException, throwIO)
import           Control.Monad       (void)
import           Data.Hashable       (Hashable)
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import           Data.HashSet        (HashSet)
import qualified Data.HashSet        as HashSet
import           Data.Semigroup
import           Data.String
import           Data.Text           (Text)
import qualified Data.Text           as Text
import           Data.Time.Clock
import           GHC.Generics        (Generic)
import           System.Exit
import           System.Process      (CreateProcess (cwd), proc,
                                      readCreateProcessWithExitCode)

import           Lodjur.Process

newtype Tag =
  Tag { unTag :: Text }
  deriving (Eq, Show, IsString)

newtype DeploymentName =
  DeploymentName { unDeploymentName :: String }
  deriving (Eq, Show, IsString, Generic, Hashable)

type JobId = Text

data DeploymentJob = DeploymentJob
  { jobId          :: JobId
  , deploymentName :: DeploymentName
  , deploymentTag  :: Tag
  } deriving (Show, Eq)

data DeployState
  = Idle
  | Deploying DeploymentJob
  deriving (Eq, Show)

data JobEvent
  = JobRunning UTCTime
  | JobFinished JobResult UTCTime
  deriving (Show, Eq)

data JobResult
  = JobSuccessful
  | JobFailed Text
  deriving (Show, Eq)

type EventLogs = HashMap JobId EventLog

type EventLog = [JobEvent]

data EventLogger = EventLogger EventLogs

data EventLogMessage r where
  -- Public messages:
  GetEventLogs :: EventLogMessage (Sync EventLogs)
  AppendEvent :: JobId -> JobEvent -> EventLogMessage Async

data Deployer = Deployer
  { state           :: DeployState
  , eventLogger     :: Ref EventLogger
  , deploymentNames :: HashSet DeploymentName
  , gitWorkingDir   :: FilePath
  }

data DeployMessage r where
  -- Public messages:
  Deploy :: DeploymentName -> Tag -> DeployMessage (Sync (Maybe DeploymentJob))
  GetCurrentState :: DeployMessage (Sync DeployState)
  GetDeploymentNames :: DeployMessage (Sync [DeploymentName])
  GetTags :: DeployMessage (Sync [Tag])
  -- Private messages:
  FinishJob :: JobResult -> DeployMessage Async

initialize :: Ref EventLogger -> HashSet DeploymentName -> FilePath -> Deployer
initialize eventLogger deploymentNames gitWorkingDir =
  Deployer {state = Idle, ..}

data GitFailed = GitFailed String String Int
  deriving (Eq, Show)

instance Exception GitFailed

data NixopsFailed = NixopsFailed String String Int
  deriving (Eq, Show)

instance Exception NixopsFailed

gitCmd :: [String] -> FilePath -> IO String
gitCmd args gitWorkingDir = do
  (exitcode, stdout, stderr) <- readCreateProcessWithExitCode
    ((proc "git" args) { cwd = Just gitWorkingDir })
    ""
  case exitcode of
    ExitSuccess      -> return stdout
    ExitFailure code -> throwIO (GitFailed stdout stderr code)

nixopsCmd :: [String] -> IO String
nixopsCmd args = do
  (exitcode, stdout, stderr) <- readCreateProcessWithExitCode
    (proc "nixops" args)
    ""
  case exitcode of
    ExitSuccess      -> return stdout
    ExitFailure code -> throwIO (NixopsFailed stdout stderr code)

deploy :: Ref EventLogger -> FilePath -> DeploymentJob -> IO JobResult
deploy eventLogger gitWorkingDir job = do
  started <- getCurrentTime
  eventLogger ! AppendEvent (jobId job) (JobRunning started)
  _ <- gitCmd ["checkout", Text.unpack (unTag (deploymentTag job)), "--recurse-submodules"] gitWorkingDir
  _ <- nixopsCmd ["deploy", "-d", unDeploymentName (deploymentName job)]
  return JobSuccessful

gitListTags :: FilePath -> IO [Tag]
gitListTags workingDir = parseTags <$> gitCmd ["tag", "-l"] workingDir
  where parseTags = map Tag . filter (not . Text.null) . Text.lines . Text.pack

notifyDeployFinished
  :: Ref Deployer
  -> Ref EventLogger
  -> DeploymentJob
  -> Either SomeException JobResult
  -> IO ()
notifyDeployFinished self eventLogger job r = do
  finished <- getCurrentTime
  let result = either (JobFailed . Text.pack . show) id r
  eventLogger ! AppendEvent (jobId job) (JobFinished result finished)
  self ! FinishJob result

instance Process Deployer where
  type Message Deployer = DeployMessage

  receive self (a@Deployer{..}, msg)=
    case (state, msg) of
      (Idle     , Deploy name tag)
        -- We require the deployment name to be known.
        | HashSet.member name deploymentNames -> do
          let job = DeploymentJob { deploymentTag = tag, jobId = "deploy-1", deploymentName = name }
          void (forkFinally (deploy eventLogger gitWorkingDir job) (notifyDeployFinished self eventLogger job))
          return (a { state = Deploying job}, Just job)
        -- We can't deploy to an unknown deployment.
        | otherwise -> do
          putStrLn ("Invalid deployment name: " <> unDeploymentName name)
          return (a, Nothing)
      (Deploying{}, Deploy{}      ) ->
        return (a, Nothing)

      -- Queries:
      (_, GetDeploymentNames) ->
        return (a, HashSet.toList deploymentNames)
      (_, GetTags) -> do
        tags <- gitListTags gitWorkingDir
        return (a, tags)
      (_, GetCurrentState) ->
        return (a, state)

      -- Private messages:
      (_, FinishJob result) -> do
        putStrLn ("Finished job with result: " <> show result)
        return a { state = Idle }

  terminate Deployer {state} = case state of
    Idle          -> return ()
    Deploying job -> putStrLn ("Killed while deploying " <> show job)

instance Process EventLogger where
  type Message EventLogger = EventLogMessage

  receive _self (a@(EventLogger logs), GetEventLogs) =
    return (a, logs)

  receive _self (EventLogger logs, AppendEvent jobid event) = do
    putStrLn ("Recording event: " <> show event)
    let events = HashMap.lookupDefault [] jobid logs
    return $ EventLogger (HashMap.insert jobid (event : events) logs)

  terminate (EventLogger _) = return ()
