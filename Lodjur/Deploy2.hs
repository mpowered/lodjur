{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
module Lodjur.Deploy2 where

import Control.Monad (void)
import Control.Concurrent
import Data.String
import Data.Text (Text)

newtype Tag = Tag { unTag :: Text } deriving (Eq, Show, IsString)

newtype DeploymentName = DeploymentName String deriving (Eq, Show, IsString)

data Message r where
  Deploy :: Tag -> Message Bool
  DeployFinished :: Tag -> Message ()

data QueuedMessage where
  Sync :: Message r -> MVar r -> QueuedMessage
  Async :: Message () -> QueuedMessage

data State = Idle | Deploying

newtype Ref = Ref { inbox :: Chan QueuedMessage }

(!) :: Ref -> Message () -> IO ()
(!) receiver msg = writeChan (inbox receiver) (Async msg)

(?) :: Ref -> Message r -> IO r
(?) receiver msg = do
  res <- newEmptyMVar
  writeChan (inbox receiver) (Sync msg res)
  takeMVar res

receive :: Ref -> (State, Message r) -> IO (State, r)
receive self = \case
  (Idle, Deploy _) -> do
    putStrLn "Deploying ..."
    void $ forkIO $ do
      threadDelay $ 5 * 1000 * 1000
      self ! DeployFinished "foo"
    return (Deploying, True)
  (Idle     , DeployFinished _) ->
    return (Idle, ())
  (Deploying, Deploy _        ) -> do
    putStrLn "Ignoring."
    return (Deploying, False)
  (Deploying, DeployFinished _) -> do
    putStrLn "Deploy finished!"
    return (Idle, ())

startServer :: IO Ref
startServer = do
  ref <- Ref <$> newChan
  let initialState = Idle
  _ <- forkIO $ receiveLoop ref initialState
  return ref
 where
  receiveLoop ref state = readChan (inbox ref) >>= \case
    Sync msg sender -> do
      (state', response) <- receive ref (state, msg)
      putMVar     sender response
      receiveLoop ref    state'
    Async msg -> do
      (state', ()) <- receive ref (state, msg)
      receiveLoop ref state'
