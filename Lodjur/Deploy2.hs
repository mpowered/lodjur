{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
module Lodjur.Deploy2 where

import Data.Semigroup
import Control.Monad (void)
import Control.Concurrent
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.String
import Data.Text (Text)

import Lodjur.Actor

newtype Tag = Tag { unTag :: Text } deriving (Eq, Show, IsString)

newtype DeploymentName = DeploymentName String deriving (Eq, Show, IsString)

data DeployState
  = Idle
  | Deploying

type DeployId = Text

newtype DeployResult = DeployResult Text deriving (Show, Eq)

type DeployHistory = HashMap DeployId DeployResult

data DeployActor = DeployActor { state :: DeployState
                               , history :: DeployHistory
                               , deploymentName :: DeploymentName
                               , gitWorkingDir :: FilePath
                               }

initialize :: DeploymentName -> FilePath -> DeployActor
initialize deploymentName gitWorkingDir =
  DeployActor { state = Idle, history = mempty, .. }

deploy :: DeployId -> IO DeployResult
deploy di = do
  threadDelay $ 5 * 1000 * 1000
  return (DeployResult (di <> " is done!"))

data DeployMessage r where
  Deploy :: Tag -> DeployMessage (Sync (Maybe DeployId))
  GetDeployStatus :: DeployId -> DeployMessage (Sync (Maybe DeployResult))
  DeployFinished :: DeployId -> DeployResult -> DeployMessage Async

instance Actor DeployActor where
  type Message DeployActor = DeployMessage

  receive self (a@DeployActor{state, history}, msg)=
    case (state, msg) of
      (Idle     , Deploy _) -> do
        putStrLn "Deploying ..."
        let di = "deploy-1"
        void $ forkIO $ do
          result <- deploy di
          self ! DeployFinished di result
        return (a { state = Deploying }, Just di)
      (Idle     , DeployFinished _ _) ->
        return a { state = Idle }
      (Deploying, Deploy _        ) -> do
        putStrLn "Ignoring."
        return (a { state = Deploying }, Nothing)
      (Deploying, DeployFinished di result) -> do
        putStrLn "Deploy finished!"
        return a { state = Idle, history = HashMap.insert di result history }
      (_, GetDeployStatus di) ->
        return (a { state = Idle }, HashMap.lookup di history)

  shutdown DeployActor {state} = case state of
    Idle -> return ()
    Deploying -> putStrLn "Killed while deploying"
