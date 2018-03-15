{-# LANGUAGE GADTs #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
module Lodjur.Deploy
  ( Tag (..)
  , DeploymentName (..)
  , JobId
  , DeploymentJob (..)
  , DeployState (..)
  , JobEvent (..)
  , EventLog
  , Deployer
  , DeployMessage (..)
  , EventLogger (..)
  , initialize
  ) where

import           Control.Concurrent
import           Control.Exception          (Exception, SomeException, throwIO)
import           Control.Monad              (void)
import           Data.HashSet               (HashSet)
import qualified Data.HashSet               as HashSet
import           Data.Hashable              (Hashable)
import           Data.Semigroup
import           Data.String
import           Data.Text                  (Text)
import qualified Data.Text                  as Text
import           Data.Time.Clock
import           GHC.Generics               (Generic)
import           System.Exit
import           System.Process             (proc, readCreateProcessWithExitCode, CreateProcess (cwd))

import           Lodjur.Process

newtype Tag =
  Tag { unTag :: Text }
  deriving (Eq, Show, IsString)

newtype DeploymentName =
  DeploymentName { unDeploymentName :: String }
  deriving (Eq, Show, IsString, Generic, Hashable)

type JobId = Text

data DeploymentJob = DeploymentJob { jobId :: JobId
                                   , deploymentName :: DeploymentName
                                   , deploymentTag :: Tag
                                   }
  deriving (Show, Eq)

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

type EventLog = [(JobId, JobEvent)]

data EventLogger = EventLogger

data EventLogMessage r where
  -- Private messages:
  AppendEvent :: JobEvent -> EventLogMessage Async

data Deployer = Deployer
  { state :: DeployState
  , eventLogger :: Ref EventLogger
  , deploymentNames :: HashSet DeploymentName
  , gitWorkingDir :: FilePath
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

deploy :: FilePath -> DeploymentJob -> IO JobResult
deploy gitWorkingDir job = do
  _ <- gitCmd ["checkout", Text.unpack (unTag (deploymentTag job)), "--recurse-submodules"] gitWorkingDir
  _ <- nixopsCmd ["deploy", "-d", unDeploymentName (deploymentName job)]
  return JobSuccessful

gitListTags :: FilePath -> IO [Tag]
gitListTags workingDir = parseTags <$> gitCmd ["tag", "-l"] workingDir
  where parseTags = map Tag . filter (not . Text.null) . Text.lines . Text.pack

notifyDeployFinished
  :: Ref Deployer
  -> Either SomeException JobResult
  -> IO ()
notifyDeployFinished self r =
  self ! FinishJob (either (JobFailed . Text.pack . show) id r)

instance Process Deployer where
  type Message Deployer = DeployMessage

  receive self (a@Deployer{..}, msg)=
    case (state, msg) of
      (Idle     , Deploy name tag)
        -- We require the deployment name to be known.
        | HashSet.member name deploymentNames -> do
          let job = DeploymentJob { deploymentTag = tag, jobId = "deploy-1", deploymentName = name }
          void (forkFinally (deploy gitWorkingDir job) (notifyDeployFinished self))
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
                 -- eventLog = (jobId (eventJob event), event) : eventLog

  terminate Deployer {state} = case state of
    Idle -> return ()
    Deploying job -> putStrLn ("Killed while deploying " <> show job)

instance Process EventLogger where
  type Message EventLogger = EventLogMessage

  receive _self (EventLogger, AppendEvent event) = do
    putStrLn ("Recording event: " <> show event)
    return EventLogger

  terminate EventLogger = return ()
