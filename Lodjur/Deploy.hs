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
  = JobRunning DeploymentJob
  | JobSuccessful DeploymentJob
  | JobFailed DeploymentJob Text
  deriving (Show, Eq)

eventJob :: JobEvent -> DeploymentJob
eventJob =
  \case
    JobRunning job -> job
    JobSuccessful job -> job
    JobFailed job _ -> job

type EventLog = [(JobId, JobEvent)]

data Deployer = Deployer { state :: DeployState
                         , eventLog :: EventLog
                         , deploymentNames :: HashSet DeploymentName
                         , gitWorkingDir :: FilePath
                         }

data DeployMessage r where
  -- Public messages:
  Deploy :: DeploymentName -> Tag -> DeployMessage (Sync (Maybe DeploymentJob))
  GetEventLog :: DeployMessage (Sync EventLog)
  GetCurrentState :: DeployMessage (Sync DeployState)
  GetTags :: DeployMessage (Sync [Tag])
  -- Private messages:
  NotifyEvent :: JobEvent -> DeployMessage Async

initialize :: HashSet DeploymentName -> FilePath -> Deployer
initialize deploymentNames gitWorkingDir =
  Deployer {state = Idle, eventLog = mempty, ..}

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

deploy :: FilePath -> DeploymentJob -> IO ()
deploy gitWorkingDir job = do
  _ <- gitCmd ["checkout", Text.unpack (jobId job), "--recurse-submodules"] gitWorkingDir
  _ <- nixopsCmd ["deploy", "-d", unDeploymentName (deploymentName job)]
  return ()

gitListTags :: FilePath -> IO [Tag]
gitListTags workingDir = parseTags <$> gitCmd ["tag", "-l"] workingDir
  where parseTags = map Tag . filter (not . Text.null) . Text.lines . Text.pack

notifyDeployFinished
  :: Ref Deployer
  -> DeploymentJob
  -> Either SomeException ()
  -> IO ()
notifyDeployFinished self d r =
  self ! NotifyEvent (either (JobFailed d . Text.pack . show) (const (JobSuccessful d)) r)

instance Process Deployer where
  type Message Deployer = DeployMessage

  receive self (a@Deployer{..}, msg)=
    case (state, msg) of
      (Idle     , Deploy name tag)
        -- We require the deployment name to be known.
        | HashSet.member name deploymentNames -> do
          let job = DeploymentJob { deploymentTag = tag, jobId = "deploy-1", deploymentName = name }
          void (forkFinally (deploy gitWorkingDir job) (notifyDeployFinished self job))
          return (a { state = Deploying job}, Just job)
        -- We can't deploy to an unknown deployment.
        | otherwise -> return (a, Nothing)
      (Idle     , NotifyEvent _) ->
        return a { state = Idle }
      (Deploying{}, Deploy{}      ) ->
        return (a, Nothing)
      (Deploying job, NotifyEvent event)
        | job == eventJob event -> do
          putStrLn ("Recorded event: " <> show event)
          return a { state = Idle, eventLog = (jobId (eventJob event), event) : eventLog }
        | otherwise -> do
          putStrLn ("Cannot record job event: " <> show event)
          return a
      (_, GetTags) -> do
        tags <- gitListTags gitWorkingDir
        return (a, tags)
      (_, GetCurrentState) ->
        return (a, state)
      (_, GetEventLog) ->
        return (a { state = Idle }, eventLog)

  terminate Deployer {state} = case state of
    Idle -> return ()
    Deploying job -> putStrLn ("Killed while deploying " <> show job)
