{-# LANGUAGE GADTs #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
module Lodjur.Deploy2
  ( Tag (..)
  , DeploymentName (..)
  , JobId
  , DeploymentJob (..)
  , DeployState (..)
  , DeployResult (..)
  , DeployActor
  , DeployMessage (..)
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
import qualified Data.Text           as Text
import           Data.Semigroup
import           Data.String
import           Data.Text           (Text)
import           GHC.Generics        (Generic)
import           System.Exit
import           System.Process      (proc, readCreateProcessWithExitCode, CreateProcess (cwd))

import Lodjur.Process

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

data DeployResult
  = DeploySuccessful DeploymentJob
  | DeployFailed DeploymentJob Text
  deriving (Show, Eq)

type DeployHistory = HashMap JobId DeployResult

data DeployActor = DeployActor { state :: DeployState
                               , history :: DeployHistory
                               , deploymentNames :: HashSet DeploymentName
                               , gitWorkingDir :: FilePath
                               }

data DeployMessage r where
  -- Public messages:
  Deploy :: DeploymentName -> Tag -> DeployMessage (Sync (Maybe DeploymentJob))
  GetResult :: JobId -> DeployMessage (Sync (Maybe DeployResult))
  GetCurrentState :: DeployMessage (Sync DeployState)
  GetTags :: DeployMessage (Sync [Tag])
  -- Private messages:
  DeployFinished :: DeploymentJob -> DeployResult -> DeployMessage Async

initialize :: HashSet DeploymentName -> FilePath -> DeployActor
initialize deploymentNames gitWorkingDir =
  DeployActor {state = Idle, history = mempty, ..}

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

deploy :: FilePath -> DeploymentJob -> IO DeployResult
deploy gitWorkingDir job = do
  _ <- gitCmd ["checkout", Text.unpack (jobId job)] gitWorkingDir
  _ <- nixopsCmd ["deploy", "-d", unDeploymentName (deploymentName job)]
  return (DeploySuccessful job)

gitListTags :: FilePath -> IO [Tag]
gitListTags workingDir = parseTags <$> gitCmd ["tag", "-l"] workingDir
  where parseTags = map Tag . filter (not . Text.null) . Text.lines . Text.pack

notifyDeployFinished
  :: Ref DeployActor
  -> DeploymentJob
  -> Either SomeException DeployResult
  -> IO ()
notifyDeployFinished self d r = do
  let result = either (DeployFailed d . Text.pack . show) id r
  self ! DeployFinished d result

instance Process DeployActor where
  type Message DeployActor = DeployMessage

  receive self (a@DeployActor{..}, msg)=
    case (state, msg) of
      (Idle     , Deploy name tag)
        -- We require the deployment name to be known.
        | HashSet.member name deploymentNames -> do
          let job = DeploymentJob { deploymentTag = tag, jobId = "deploy-1", deploymentName = name }
          void (forkFinally (deploy gitWorkingDir job) (notifyDeployFinished self job))
          return (a { state = Deploying job}, Just job)
        -- We can't deploy to an unknown deployment.
        | otherwise -> return (a, Nothing)
      (Idle     , DeployFinished _ _) ->
        return a { state = Idle }
      (Deploying{}, Deploy{}      ) ->
        return (a, Nothing)
      (Deploying job, DeployFinished finishedJob result)
        | job == finishedJob -> do
          putStrLn ("Deploy job finished: " <> show job)
          return a { state = Idle, history = HashMap.insert (jobId job) result history }
        | otherwise -> do
          putStrLn ("Cannot mark deploy job as finished: " <> show job)
          return a
      (_, GetTags) -> do
        tags <- gitListTags gitWorkingDir
        return (a, tags)
      (_, GetCurrentState) ->
        return (a, state)
      (_, GetResult di) ->
        return (a { state = Idle }, HashMap.lookup di history)

  terminate DeployActor {state} = case state of
    Idle -> return ()
    Deploying job -> putStrLn ("Killed while deploying " <> show job)
