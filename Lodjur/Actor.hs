{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
module Lodjur.Actor where

import Control.Concurrent

class Actor a where
  data Message a :: * -> *

  initialState :: a
  receive :: Ref a -> (a, Message a r) -> IO (a, r)

data QueuedMessage m where
  Sync :: m r -> MVar r -> QueuedMessage m
  Async :: m () -> QueuedMessage m

newtype Ref a = Ref { inbox :: Chan (QueuedMessage (Message a)) }

(!) :: Actor a => Ref a -> Message a () -> IO ()
(!) receiver msg = writeChan (inbox receiver) (Async msg)

(?) :: Actor a => Ref a -> Message a r -> IO r
(?) receiver msg = do
  res <- newEmptyMVar
  writeChan (inbox receiver) (Sync msg res)
  takeMVar res

spawn :: Actor a => IO (Ref a)
spawn = do
  ref <- Ref <$> newChan
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
