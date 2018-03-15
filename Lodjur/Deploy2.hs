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

import Control.Exception (SomeException)
import Data.Semigroup
import Control.Monad (void)
import Control.Concurrent
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.String
import Data.Text (Text)
import qualified Data.Text as Text
import Data.HashSet (HashSet)
import qualified Data.HashSet as HashSet
import GHC.Generics (Generic)
import Data.Hashable (Hashable)

import Lodjur.Process

newtype Tag = Tag { unTag :: Text } deriving (Eq, Show, IsString)

newtype DeploymentName =
  DeploymentJobName String
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
  -- Private messages:
  DeployFinished :: DeploymentJob -> DeployResult -> DeployMessage Async

initialize :: HashSet DeploymentName -> FilePath -> DeployActor
initialize deploymentNames gitWorkingDir =
  DeployActor {state = Idle, history = mempty, ..}

deploy :: DeploymentJob -> IO DeployResult
deploy d = do
  threadDelay $ 5 * 1000 * 1000
  return (DeploySuccessful d)

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

  receive self (a@DeployActor{state, history, deploymentNames}, msg)=
    case (state, msg) of
      (Idle     , Deploy name tag)
        -- We require the deployment name to be known.
        | HashSet.member name deploymentNames -> do
          let d = DeploymentJob { deploymentTag = tag, jobId = "deploy-1", deploymentName = name }
          void (forkFinally (deploy d) (notifyDeployFinished self d))
          return (a { state = Deploying d}, Just d)
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
      (_, GetCurrentState) ->
        return (a, state)
      (_, GetResult di) ->
        return (a { state = Idle }, HashMap.lookup di history)

  terminate DeployActor {state} = case state of
    Idle -> return ()
    Deploying job -> putStrLn ("Killed while deploying " <> show job)
