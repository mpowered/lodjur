{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Database.Redis.Exception where

import           Control.Exception
import           Control.Monad.IO.Class
import           Database.Redis         (Redis)
import qualified Database.Redis         as Redis

data RedisException
  = RedisError Redis.Reply
  | RedisTxError String
  | RedisTxAborted
  | DecodeError String
  deriving (Show, Eq)

instance Exception RedisException

class RedisCheck m f where
  check :: MonadIO m => m (f a) -> m a

instance RedisCheck Redis (Either Redis.Reply) where
  check x = do
    r <- x
    either (liftIO . throwIO . RedisError) return r

instance RedisCheck Redis Redis.TxResult where
  check x = do
    r <- x
    case r of
      Redis.TxSuccess a -> return a
      Redis.TxAborted -> liftIO $ throwIO RedisTxAborted
      Redis.TxError err -> liftIO $ throwIO $ RedisTxError err
