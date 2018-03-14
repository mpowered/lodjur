{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
module Lodjur.Deploy2 where

import Control.Monad (void)
import Control.Concurrent
import Data.String
import Data.Text (Text)

import Lodjur.Actor

newtype Tag = Tag { unTag :: Text } deriving (Eq, Show, IsString)

newtype DeploymentName = DeploymentName String deriving (Eq, Show, IsString)

data DeployActor
  = Idle
  | Deploying

instance Actor DeployActor where
  data Message DeployActor r where
    Deploy :: Tag -> Message DeployActor (Sync Bool)
    DeployFinished :: Tag -> Message DeployActor Async

  initialState = Idle

  receive self = \case
    (Idle     , Deploy _) -> do
      putStrLn "Deploying ..."
      void $ forkIO $ do
        threadDelay $ 5 * 1000 * 1000
        self ! DeployFinished "foo"
      return (Deploying, True)
    (Idle     , DeployFinished _) ->
      return Idle
    (Deploying, Deploy _        ) -> do
      putStrLn "Ignoring."
      return (Deploying, False)
    (Deploying, DeployFinished _) -> do
      putStrLn "Deploy finished!"
      return Idle
