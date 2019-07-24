{-# LANGUAGE GADTs             #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
module Lodjur.Deployment.Deployer
  ( DeploymentName (..)
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
import           Control.Exception           (Exception, SomeException, throwIO)
import           Control.Monad               (void)
import qualified Data.Text                   as Text
import           Data.Time.Clock
import qualified Data.UUID                   as UUID
import qualified Data.UUID.V4                as UUID
import           System.Exit
import           System.Process

import           Lodjur.Database             (DbPool)
import           Lodjur.Deployment
import qualified Lodjur.Deployment.Database  as Database
import           Lodjur.Events.EventLogger   (EventLogMessage (..), EventLogger,
                                              JobEvent (..))
import qualified Lodjur.Git                  as Git
import           Lodjur.Git.GitAgent         (GitAgent, GitAgentMessage (..))
import           Lodjur.Output.OutputLogger  (OutputLogMessage (..),
                                              OutputLogger,
                                              logCreateProcessWithExitCode)
import           Lodjur.Output.OutputLoggers (OutputLoggers)
import qualified Lodjur.Output.OutputLoggers as OutputLoggers
import           Lodjur.Process
import           Lodjur.User

data DeployState
  = Idle
  | Deploying DeploymentJob
  deriving (Eq, Show)

type DeploymentJobs = [(DeploymentJob, Maybe JobResult)]

data Deployer = Deployer
  { state           :: DeployState
  , eventLogger     :: Ref EventLogger
  , outputLoggers   :: Ref OutputLoggers
  , gitAgent        :: Ref GitAgent
  , deployments     :: [Deployment]
  , pool            :: DbPool
  }

data DeployMessage r where
  -- Public messages:
  Deploy :: DeploymentName -> Git.Revision -> UTCTime -> Bool -> User -> DeployMessage (Sync (Maybe DeploymentJob))
  GetCurrentState :: DeployMessage (Sync DeployState)
  GetJob :: JobId -> DeployMessage (Sync (Maybe (DeploymentJob, Maybe JobResult)))
  GetJobs :: Maybe Word -> DeployMessage (Sync DeploymentJobs)
  GetDeployments :: Role -> DeployMessage (Sync [Deployment])
  -- Private messages:
  FinishJob :: DeploymentJob -> JobResult -> DeployMessage Async

initialize
  :: Ref EventLogger
  -> Ref OutputLoggers
  -> Ref GitAgent
  -> [Deployment]
  -> DbPool
  -> IO Deployer
initialize eventLogger outputLoggers gitAgent deployments pool = do
  Database.initialize pool
  return Deployer {state = Idle, ..}

data GitFailed = GitFailed String String Int
  deriving (Eq, Show)

instance Exception GitFailed

data NixopsFailed = NixopsFailed String String Int
  deriving (Eq, Show)

instance Exception NixopsFailed

nixopsCmdLogged :: Ref OutputLogger -> [String] -> IO String
nixopsCmdLogged outputLogger args = do
  exitcode <- logCreateProcessWithExitCode outputLogger (proc "nixops" args)
  case exitcode of
    ExitSuccess      -> return ""
    ExitFailure code -> throwIO (NixopsFailed "" "" code)

deploy
  :: Ref EventLogger
  -> Ref OutputLogger
  -> Ref GitAgent
  -> DeploymentJob
  -> [String]
  -> IO JobResult
deploy eventLogger outputLogger gitAgent job args = do
  started <- getCurrentTime
  eventLogger ! AppendEvent (jobId job) (JobRunning started)
  _ <- gitAgent ? Checkout (deploymentRevision job) outputLogger
  _ <- nixopsCmdLogged outputLogger $
                       ["deploy", "-d", Text.unpack (unDeploymentName (deploymentJobName job))]
                       ++ args
  return JobSuccessful

notifyDeployFinished
  :: Ref Deployer
  -> Ref EventLogger
  -> Ref OutputLogger
  -> DeploymentJob
  -> Either SomeException JobResult
  -> IO ()
notifyDeployFinished self eventLogger logger job r = do
  finished <- getCurrentTime
  let result = either (JobFailed . Text.pack . show) id r
  eventLogger ! AppendEvent (jobId job) (JobFinished result finished)
  logger ! OutputFence
  kill logger
  self ! FinishJob job result

instance Process Deployer where
  type Message Deployer = DeployMessage

  receive self (a@Deployer{..}, msg)=
    case (state, msg) of
      (Idle     , Deploy name deploymentRevision deploymentTime deploymentBuildOnly user) -> do
        case lookup name (zip (map deploymentName deployments) deployments) of
          Just deployment -> do
            if canDeploy (userRole user) deployment
             then do
              jobId <- UUID.toText <$> UUID.nextRandom
              let deploymentJobStartedBy = userId user
              let job = DeploymentJob {deploymentJobName = name, ..}
              logger <- outputLoggers ? OutputLoggers.SpawnOutputLogger jobId
              let args = if deploymentBuildOnly then ["--build-only"] else []
              void (forkFinally (deploy eventLogger logger gitAgent job args) (notifyDeployFinished self eventLogger logger job))
              Database.insertJob pool job Nothing
              return ( a { state = Deploying job } , Just job)
             else do
              putStrLn "User is not in the correct GitHub team to deploy to this deployment"
              return (a, Nothing)
          Nothing -> do
            putStrLn ("Invalid deployment name: " <> Text.unpack (unDeploymentName name))
            return (a, Nothing)
      (Deploying{}, Deploy{}      ) ->
        return (a, Nothing)

      -- Queries:
      (_, GetDeployments role) ->
        return (a, filter (canDeploy role) deployments)
      (_, GetJob jobId) -> do
        job <- Database.getJobById pool jobId
        return (a, job)
      (_, GetJobs maxCount) -> do
        jobs <- Database.getAllJobs pool maxCount
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

canDeploy :: Role -> Deployment -> Bool
canDeploy Core   _ = True
canDeploy Tester d = deploymentTest d
