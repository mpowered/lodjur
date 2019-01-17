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
  , DeploymentType (..)
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
import           Control.Monad               (void, when)
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
  , gitWorkDir      :: FilePath
  , deployments     :: [Deployment]
  , pool            :: DbPool
  }

data DeployMessage r where
  -- Public messages:
  Deploy :: DeploymentType -> DeploymentName -> Git.Revision -> UTCTime -> UserId -> DeployMessage (Sync (Maybe DeploymentJob))
  GetCurrentState :: DeployMessage (Sync DeployState)
  GetJob :: JobId -> DeployMessage (Sync (Maybe (DeploymentJob, Maybe JobResult)))
  GetJobs :: Maybe Word -> DeployMessage (Sync DeploymentJobs)
  GetDeployments :: DeployMessage (Sync [Deployment])
  -- Private messages:
  FinishJob :: DeploymentJob -> JobResult -> DeployMessage Async

initialize
  :: Ref EventLogger
  -> Ref OutputLoggers
  -> Ref GitAgent
  -> FilePath
  -> [Deployment]
  -> DbPool
  -> IO Deployer
initialize eventLogger outputLoggers gitAgent gitWorkDir deployments pool = do
  Database.initialize pool
  return Deployer {state = Idle, ..}

data GitFailed = GitFailed String String Int
  deriving (Eq, Show)

instance Exception GitFailed

data NixopsFailed = NixopsFailed String String Int
  deriving (Eq, Show)

instance Exception NixopsFailed

data CheckFailed = CheckFailed Int
  deriving (Eq, Show)

instance Exception CheckFailed

data Check = DoCheck | NoCheck
  deriving (Eq, Show)

nixopsCmdLogged :: Ref OutputLogger -> [String] -> IO String
nixopsCmdLogged outputLogger args = do
  exitcode <- logCreateProcessWithExitCode outputLogger (proc "nixops" args)
  case exitcode of
    ExitSuccess      -> return ""
    ExitFailure code -> throwIO (NixopsFailed "" "" code)

checkLogged :: Ref OutputLogger -> [String] -> FilePath -> IO String
checkLogged outputLogger args gitWorkingDir = do
  exitcode <- logCreateProcessWithExitCode
    outputLogger
    ((proc "./check.sh" args) { cwd = Just gitWorkingDir })
  case exitcode of
    ExitSuccess      -> return ""
    ExitFailure code -> throwIO (CheckFailed code)

deploy
  :: Ref EventLogger
  -> Ref OutputLogger
  -> Ref GitAgent
  -> FilePath
  -> DeploymentJob
  -> [String]
  -> Check
  -> IO JobResult
deploy eventLogger outputLogger gitAgent gitWorkDir job args check = do
  started <- getCurrentTime
  eventLogger ! AppendEvent (jobId job) (JobRunning started)
  _ <- gitAgent ? Checkout (deploymentRevision job) outputLogger
  _ <- nixopsCmdLogged outputLogger $
                       ["deploy", "-d", Text.unpack (unDeploymentName (deploymentJobName job))]
                       ++ args
  when (check == DoCheck) $
      void $ checkLogged outputLogger [] gitWorkDir
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
      (Idle     , Deploy deploymentType name deploymentRevision deploymentTime deploymentJobStartedBy)
        -- We require the deployment name to be known.
        | elem name (map deploymentName deployments) -> do
          jobId <- UUID.toText <$> UUID.nextRandom
          let job = DeploymentJob {deploymentJobName = name, ..}
          logger <- outputLoggers ? OutputLoggers.SpawnOutputLogger jobId
          let (args, check) =
                case deploymentType of
                  BuildOnly   -> (["--build-only"], NoCheck)
                  BuildCheck  -> (["--build-only"], DoCheck)
                  BuildDeploy -> ([], NoCheck)
          void $ forkFinally
            (deploy eventLogger logger gitAgent gitWorkDir job args check)
            (notifyDeployFinished self eventLogger logger job)
          Database.insertJob pool job Nothing
          return ( a { state = Deploying job } , Just job)
        -- We can't deploy to an unknown deployment.
        | otherwise -> do
          putStrLn ("Invalid deployment name: " <> Text.unpack (unDeploymentName name))
          return (a, Nothing)
      (Deploying{}, Deploy{}      ) ->
        return (a, Nothing)

      -- Queries:
      (_, GetDeployments) ->
        return (a, deployments)
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
