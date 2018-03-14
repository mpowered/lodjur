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
import qualified Data.Text.IO as Text

import Lodjur.Actor

newtype Tag = Tag { unTag :: Text } deriving (Eq, Show, IsString)

newtype DeploymentName = DeploymentName String deriving (Eq, Show, IsString)

type DeployId = Text

data DeployState
  = Idle
  | Deploying Tag DeployId

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
  -- Public messages:
  Deploy :: Tag -> DeployMessage (Sync (Maybe DeployId))
  GetResult :: DeployId -> DeployMessage (Sync (Maybe DeployResult))
  GetCurrentState :: DeployMessage (Sync DeployState)
  -- Private messages:
  DeployFinished :: DeployId -> DeployResult -> DeployMessage Async

instance Actor DeployActor where
  type Message DeployActor = DeployMessage

  receive self (a@DeployActor{state, history}, msg)=
    case (state, msg) of
      (Idle     , Deploy tag) -> do
        let di = "deploy-1"
        Text.putStrLn ("Deploying tag " <> unTag tag <> ". Deploy ID: " <> di)
        void $ forkIO $ do
          result <- deploy di
          self ! DeployFinished di result
        return (a { state = Deploying tag di}, Just di)
      (Idle     , DeployFinished _ _) ->
        return a { state = Idle }
      (Deploying{}, Deploy _        ) -> do
        Text.putStrLn "Ignoring."
        return (a, Nothing)
      (Deploying _ currentId, DeployFinished di result)
        | currentId == di -> do
          Text.putStrLn ("Deploy finished: " <> di)
          return a { state = Idle, history = HashMap.insert di result history }
        | otherwise -> do
          Text.putStrLn ("Cannot mark deploy as finished: " <> di)
          return a
      (_, GetCurrentState) ->
        return (a, state)
      (_, GetResult di) ->
        return (a { state = Idle }, HashMap.lookup di history)

  shutdown DeployActor {state} = case state of
    Idle -> return ()
    Deploying tag _ -> putStrLn ("Killed while deploying " <> show tag)
