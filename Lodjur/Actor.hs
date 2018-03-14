{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE EmptyDataDecls #-}
module Lodjur.Actor where

import Control.Concurrent

class Actor a where
  data Message a :: * -> *

  initialState :: a
  receive :: Ref a -> (a, Message a r) -> IO (ReceiveType a r)

data Sync r
data Async

data QueuedMessage m where
  SyncMessage :: m (Sync r) -> MVar r -> QueuedMessage m
  AsyncMessage :: m Async -> QueuedMessage m

type family ReceiveType a r where
  ReceiveType a Async = a
  ReceiveType a (Sync r) = (a, r)

newtype Ref a = Ref { inbox :: Chan (QueuedMessage (Message a)) }

(!) :: Actor a => Ref a -> Message a Async -> IO ()
(!) receiver msg = writeChan (inbox receiver) (AsyncMessage msg)

(?) :: Actor a => Ref a -> Message a (Sync r) -> IO r
(?) receiver msg = do
  res <- newEmptyMVar
  writeChan (inbox receiver) (SyncMessage msg res)
  takeMVar res

spawn :: Actor a => IO (Ref a)
spawn = do
  ref <- Ref <$> newChan
  _ <- forkIO $ receiveLoop ref initialState
  return ref
 where
  receiveLoop ref state = readChan (inbox ref) >>= \case
    SyncMessage msg sender -> do
      (state', response) <- receive ref (state, msg)
      putMVar     sender response
      receiveLoop ref    state'
    AsyncMessage msg -> do
      state' <- receive ref (state, msg)
      receiveLoop ref state'
