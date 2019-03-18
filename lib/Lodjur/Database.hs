{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module Lodjur.Database where

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
