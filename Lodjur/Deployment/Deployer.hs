{-# LANGUAGE GADTs             #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
module Lodjur.Deployment.Deployer
  ( Tag (..)
  , DeploymentName (..)
  , JobId
  , DeploymentJob (..)
  , DeploymentJobs
  , DeployState (..)
  , JobEvent (..)
  , JobResult (..)
  , Deployer
  , DeployMessage (..)
  , initialize
  ) where

import           Control.Concurrent
import           Control.Exception          (Exception, SomeException, throwIO)
import           Control.Monad              (void)
import           Data.HashSet               (HashSet)
import qualified Data.HashSet               as HashSet
import           Data.Semigroup
import qualified Data.Text                  as Text
import           Data.Time.Clock
import qualified Data.UUID                  as UUID
import qualified Data.UUID.V4               as UUID
import           System.Exit
import           System.Process

import           Lodjur.Database            (DbPool)
import           Lodjur.Deployment
import qualified Lodjur.Deployment.Database as Database
import           Lodjur.Events.EventLogger  (EventLogMessage (..), EventLogger,
                                             JobEvent (..))
import           Lodjur.Git
import           Lodjur.Git.GitAgent        (GitAgent, GitAgentMessage (..))
import           Lodjur.Output.OutputLogger (OutputLogger,
                                             logCreateProcessWithExitCode)
import           Lodjur.Process

data DeployState
  = Idle
  | Deploying DeploymentJob
  deriving (Eq, Show)

type DeploymentJobs = [(DeploymentJob, Maybe JobResult)]

data Deployer = Deployer
  { state           :: DeployState
  , eventLogger     :: Ref EventLogger
  , outputLogger    :: Ref OutputLogger
  , gitAgent        :: Ref GitAgent
  , deploymentNames :: HashSet DeploymentName
  , pool            :: DbPool
  }

data DeployMessage r where
  -- Public messages:
  Deploy :: DeploymentName -> Tag -> UTCTime -> DeployMessage (Sync (Maybe DeploymentJob))
  GetCurrentState :: DeployMessage (Sync DeployState)
  GetJobs :: DeployMessage (Sync DeploymentJobs)
  GetDeploymentNames :: DeployMessage (Sync [DeploymentName])
  -- Private messages:
  FinishJob :: DeploymentJob -> JobResult -> DeployMessage Async

initialize
  :: Ref EventLogger
  -> Ref OutputLogger
  -> Ref GitAgent
  -> HashSet DeploymentName
  -> DbPool
  -> IO Deployer
initialize eventLogger outputLogger gitAgent deploymentNames pool = do
  Database.initialize pool
  return Deployer {state = Idle, ..}

data GitFailed = GitFailed String String Int
  deriving (Eq, Show)

instance Exception GitFailed

data NixopsFailed = NixopsFailed String String Int
  deriving (Eq, Show)

instance Exception NixopsFailed

nixopsCmdLogged :: Ref OutputLogger -> JobId -> [String] -> IO String
nixopsCmdLogged outputLogger jobid args = do
  exitcode <- logCreateProcessWithExitCode outputLogger
                                           jobid
                                           (proc "nixops" args)
  case exitcode of
    ExitSuccess      -> return ""
    ExitFailure code -> throwIO (NixopsFailed "" "" code)

deploy
  :: Ref EventLogger
  -> Ref OutputLogger
  -> Ref GitAgent
  -> DeploymentJob
  -> IO JobResult
deploy eventLogger outputLogger gitAgent job = do
  started <- getCurrentTime
  eventLogger ! AppendEvent (jobId job) (JobRunning started)
  _ <- gitAgent ? Checkout (deploymentTag job) outputLogger (jobId job)
  _ <- nixopsCmdLogged outputLogger
                       (jobId job)
                       ["deploy", "-d", unDeploymentName (deploymentName job)]
  return JobSuccessful

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
  self ! FinishJob job result

instance Process Deployer where
  type Message Deployer = DeployMessage

  receive self (a@Deployer{..}, msg)=
    case (state, msg) of
      (Idle     , Deploy deploymentName deploymentTag deploymentTime)
        -- We require the deployment name to be known.
        | HashSet.member deploymentName deploymentNames -> do
          jobId <- UUID.toText <$> UUID.nextRandom
          let job = DeploymentJob {..}
          void (forkFinally (deploy eventLogger outputLogger gitAgent job) (notifyDeployFinished self eventLogger job))
          Database.insertJob pool job Nothing
          return ( a { state = Deploying job } , Just job)
        -- We can't deploy to an unknown deployment.
        | otherwise -> do
          putStrLn ("Invalid deployment name: " <> unDeploymentName deploymentName)
          return (a, Nothing)
      (Deploying{}, Deploy{}      ) ->
        return (a, Nothing)

      -- Queries:
      (_, GetDeploymentNames) ->
        return (a, HashSet.toList deploymentNames)
      (_, GetJobs) -> do
        jobs <- Database.getAllJobs pool
        return (a, jobs)
      (_, GetCurrentState) ->
        return (a, state)

      -- Private messages:
      (_, FinishJob job result) -> do
        Database.updateJobResult pool (jobId job) result
        return a { state = Idle }

  terminate Deployer {state} = case state of
    Idle          -> return ()
    Deploying job -> putStrLn ("Killed while deploying " <> show job)
