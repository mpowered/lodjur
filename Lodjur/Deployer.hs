{-# LANGUAGE GADTs             #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
module Lodjur.Deployer
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
import           Control.Exception   (Exception, SomeException, throwIO, tryJust)
import           Control.Monad       (void)
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import           Data.HashSet        (HashSet)
import qualified Data.HashSet        as HashSet
import           Data.Semigroup
import qualified Data.Text           as Text
import           Data.Time.Clock
import           System.Exit
import           System.IO
import           System.IO.Error     (isEOFError)
import           System.Process
import           Lodjur.Deployment
import           Lodjur.EventLogger
import           Lodjur.OutputLogger
import           Lodjur.Process

data DeployState
  = Idle
  | Deploying DeploymentJob
  deriving (Eq, Show)

type DeploymentJobs = HashMap JobId (DeploymentJob, Maybe JobResult)
data Deployer = Deployer
  { state           :: DeployState
  , eventLogger     :: Ref EventLogger
  , outputLogger    :: Ref OutputLogger
  , deploymentNames :: HashSet DeploymentName
  , gitWorkingDir   :: FilePath
  , jobs            :: DeploymentJobs
  }

data DeployMessage r where
  -- Public messages:
  Deploy :: DeploymentName -> Tag -> DeployMessage (Sync (Maybe DeploymentJob))
  GetCurrentState :: DeployMessage (Sync DeployState)
  GetJobs :: DeployMessage (Sync DeploymentJobs)
  GetDeploymentNames :: DeployMessage (Sync [DeploymentName])
  GetTags :: DeployMessage (Sync [Tag])
  -- Private messages:
  FinishJob :: DeploymentJob -> JobResult -> DeployMessage Async

initialize :: Ref EventLogger -> Ref OutputLogger -> HashSet DeploymentName -> FilePath -> Deployer
initialize eventLogger outputLogger deploymentNames gitWorkingDir =
  Deployer {state = Idle, jobs = mempty, ..}

data GitFailed = GitFailed String String Int
  deriving (Eq, Show)

instance Exception GitFailed

data NixopsFailed = NixopsFailed String String Int
  deriving (Eq, Show)

instance Exception NixopsFailed

logCreateProcessWithExitCode
    :: Ref OutputLogger
    -> JobId
    -> CreateProcess
    -> IO ExitCode
logCreateProcessWithExitCode outputLogger jobid cp = do
  let cp_opts = cp {
                  std_in  = NoStream,
                  std_out = CreatePipe,
                  std_err = CreatePipe
                }

  (_, Just hout, Just herr, ph) <- createProcess cp_opts
  void $ logStream outputLogger jobid hout
  void $ logStream outputLogger jobid herr
  waitForProcess ph

logStream :: Ref OutputLogger -> JobId -> Handle -> IO ThreadId
logStream logger jobid h = forkIO go
 where
  go = do
    next <- tryJust
              (\e -> if isEOFError e then Just () else Nothing)
              (hGetLine h)
    case next of
      Left _ -> return ()
      Right line -> do
        logger ! AppendOutput jobid [line]
        go

gitCmd :: [String] -> FilePath -> IO String
gitCmd args gitWorkingDir = do
  (exitcode, out, err) <- readCreateProcessWithExitCode
    ((proc "git" args) { cwd = Just gitWorkingDir }) ""
  case exitcode of
    ExitSuccess      -> return out
    ExitFailure code -> throwIO (GitFailed out err code)

gitCmdLogged :: Ref OutputLogger -> JobId -> [String] -> FilePath -> IO String
gitCmdLogged outputLogger jobid args gitWorkingDir = do
  exitcode <- logCreateProcessWithExitCode outputLogger jobid
    ((proc "git" args) { cwd = Just gitWorkingDir })
  case exitcode of
    ExitSuccess      -> return ""
    ExitFailure code -> throwIO (GitFailed "" "" code)

nixopsCmdLogged :: Ref OutputLogger -> JobId -> [String] -> IO String
nixopsCmdLogged outputLogger jobid args = do
  exitcode <- logCreateProcessWithExitCode outputLogger jobid
    (proc "nixops" args)
  case exitcode of
    ExitSuccess      -> return ""
    ExitFailure code -> throwIO (NixopsFailed "" "" code)

deploy :: Ref EventLogger -> Ref OutputLogger -> FilePath -> DeploymentJob -> IO JobResult
deploy eventLogger outputLogger gitWorkingDir job = do
  started <- getCurrentTime
  eventLogger ! AppendEvent (jobId job) (JobRunning started)
  _ <- gitCmdLogged outputLogger (jobId job)
    [ "checkout"
    , Text.unpack (unTag (deploymentTag job))
    , "--recurse-submodules"
    ]
    gitWorkingDir
  _ <- nixopsCmdLogged outputLogger (jobId job)
    ["deploy", "-d", unDeploymentName (deploymentName job)]
  return JobSuccessful

gitListTags :: FilePath -> IO [Tag]
gitListTags workingDir =
  parseTags <$> gitCmd ["tag", "-l"] workingDir
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
  self ! FinishJob job result

instance Process Deployer where
  type Message Deployer = DeployMessage

  receive self (a@Deployer{..}, msg)=
    case (state, msg) of
      (Idle     , Deploy name tag)
        -- We require the deployment name to be known.
        | HashSet.member name deploymentNames -> do
          let job = DeploymentJob { deploymentTag = tag, jobId = "deploy-1", deploymentName = name }
          void (forkFinally (deploy eventLogger outputLogger gitWorkingDir job) (notifyDeployFinished self eventLogger job))
          return ( a { state = Deploying job
                     , jobs = HashMap.insert (jobId job) (job, Nothing) jobs
                     }
                 , Just job
                 )
        -- We can't deploy to an unknown deployment.
        | otherwise -> do
          putStrLn ("Invalid deployment name: " <> unDeploymentName name)
          return (a, Nothing)
      (Deploying{}, Deploy{}      ) ->
        return (a, Nothing)

      -- Queries:
      (_, GetDeploymentNames) ->
        return (a, HashSet.toList deploymentNames)
      (_, GetJobs) ->
        return (a, jobs)
      (_, GetTags) -> do
        tags <- gitListTags gitWorkingDir
        return (a, tags)
      (_, GetCurrentState) ->
        return (a, state)

      -- Private messages:
      (_, FinishJob job result) ->
        return a { state = Idle
                 , jobs = HashMap.insert (jobId job) (job, Just result) jobs
                 }

  terminate Deployer {state} = case state of
    Idle          -> return ()
    Deploying job -> putStrLn ("Killed while deploying " <> show job)
