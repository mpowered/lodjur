{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module Lodjur.Database
  ( lookup
  , put
  , put_
  , del
  , del_
  , adjust
  , modify
  )
where

import           Control.Monad
import           Control.Exception
import           Control.Monad.IO.Class
import           Data.Aeson                 (FromJSON, ToJSON)
import qualified Data.Aeson                 as Aeson
import           Data.ByteString            (ByteString)
import qualified Data.ByteString.Lazy       as BS
import           Database.Redis             (Redis)
import qualified Database.Redis             as Redis
import qualified Database.Redis.Exception   as Redis

import           Lodjur.Database.Types

import           Prelude                    hiding (lookup)

decode :: (MonadIO m, FromJSON a) => ByteString -> m a
decode x =
  case Aeson.eitherDecodeStrict' x of
    Left string -> liftIO $ throwIO $ DecodeError string
    Right a -> return a

lookup :: (HasKey a, FromJSON a) => Key a -> Redis (Maybe a)
lookup key = do
  bs <- Redis.check $ Redis.get (redisKey key)
  traverse decode bs

put :: (HasKey a, ToJSON a) => Key a -> a -> Redis Bool
put key a = do
  r <- Redis.check $ Redis.set (redisKey key) (BS.toStrict . Aeson.encode $ a)
  return $ r == Redis.Ok

put_ :: (HasKey a, ToJSON a) => Key a -> a -> Redis ()
put_ key a = void $ put key a

del :: HasKey a => Key a -> Redis Bool
del key = do
  r <- Redis.check $ Redis.del [redisKey key]
  return $ r == 1

del_ :: HasKey a => Key a -> Redis ()
del_ key = void $ del key

adjust :: (HasKey a, FromJSON a, ToJSON a) => Key a -> (Maybe a -> Maybe a) -> Redis ()
adjust key f = do
  void $ Redis.check $ Redis.watch [redisKey key]
  a <- lookup key
  case (a, f a) of
    (_, Just v) ->
      void $ Redis.check $ Redis.multiExec $
        Redis.set (redisKey key) (BS.toStrict . Aeson.encode $ v)
    (Just _, Nothing) ->
      void $ Redis.check $ Redis.multiExec $
        Redis.del [redisKey key]
    _ ->
      return ()
  void $ Redis.check Redis.unwatch

modify :: (HasKey a, FromJSON a, ToJSON a) => Key a -> (a -> a) -> Redis ()
modify key f = adjust key $
  \case
    Just a -> Just (f a)
    Nothing -> Nothing
