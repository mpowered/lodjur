{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Database.Redis.Queue
  ( Queue (..)
  , MsgId (..)
  , RedisError (..)
  , randomId
  , push
  , pop
  , poppush
  , move
  , remove
  , lookupEither
  , lookup
  , setTtl
  )
where

import           Control.Exception
import           Control.Monad
import           Control.Monad.IO.Class
import           Database.Redis                as Redis hiding (move)
import           Data.Aeson                    as Aeson
import           Data.ByteString               (ByteString)
import           Data.ByteString.Lazy          (toStrict)
import           Data.Text                     (Text)
import qualified Data.Text.Encoding            as Text
import           Data.UUID                     (UUID)
import qualified Data.UUID                     as UUID
import qualified Data.UUID.V4                  as UUID
import           GHC.Generics
import           Prelude                       hiding (lookup)

newtype Queue = Queue { unQueue :: Text }
  deriving (Show, Eq, Ord, Generic)

newtype MsgId = MsgId { unMsgId :: Text }
  deriving (Show, Eq, Ord, Generic)

instance ToJSON MsgId where
  toJSON = toJSON . unMsgId

instance FromJSON MsgId where
  parseJSON v = MsgId <$> parseJSON v

data RedisError
  = RedisError Reply
  | RedisTxError String
  | RedisTxAborted
  deriving (Show, Eq)

instance Exception RedisError

queueKey :: Queue -> ByteString
queueKey = Text.encodeUtf8 . unQueue

msgIdKey :: MsgId -> ByteString
msgIdKey = Text.encodeUtf8 . unMsgId

msgIdFromBS :: ByteString -> MsgId
msgIdFromBS = MsgId . Text.decodeUtf8

toMsgId :: UUID -> MsgId
toMsgId a = MsgId (UUID.toText a)

randomId :: IO MsgId
randomId = toMsgId <$> UUID.nextRandom

encode' :: ToJSON a => a -> ByteString
encode' = toStrict . encode

class RedisCheck m f where
  check :: MonadIO m => m (f a) -> m a

instance RedisCheck Redis (Either Reply) where
  check x = do
    r <- x
    either (liftIO . throwIO . RedisError) return r

instance RedisCheck Redis TxResult where
  check x = do
    r <- x
    case r of
      TxSuccess a -> return a
      TxAborted -> liftIO $ throwIO RedisTxAborted
      TxError err -> liftIO $ throwIO $ RedisTxError err

push :: ToJSON a => Queue -> [a] -> Redis [MsgId]
push name msgs = do
  ids <- liftIO $ mapM (const randomId) msgs
  let keys' = map msgIdKey ids
  (Ok, _added) <-
    check $ multiExec $ do
      status <- mset (zip keys' (map encode' msgs))
      added <- lpush (queueKey name) keys'
      return $ (,) <$> status <*> added
  return ids

setTtl :: [MsgId] -> Integer -> Redis ()
setTtl msgids expiry = do
  let keys' = map msgIdKey msgids
  mapM_ (\k -> check $ expire k expiry) keys'

pop :: Queue -> Integer -> Redis (Maybe MsgId)
pop name timeout =
  fmap (msgIdFromBS . snd) <$> check (brpop [queueKey name] timeout)

poppush :: Queue -> Queue -> Integer -> Redis (Maybe MsgId)
poppush src dst timeout =
  fmap msgIdFromBS <$> check (brpoplpush (queueKey src) (queueKey dst) timeout)

move :: Queue -> Queue -> MsgId -> Redis ()
move src dst msgid = do
  let key = msgIdKey msgid
  matched <- check $ lrem (queueKey src) 0 key
  void $ check $ lpush (queueKey dst) (replicate (fromInteger matched) key)

remove :: Queue -> MsgId -> Redis ()
remove name msgid = do
  let key = msgIdKey msgid
  void $ check $
    multiExec $ do
      void $ del [key]
      lrem (queueKey name) 0 key

lookupEither :: FromJSON a => MsgId -> Redis (Maybe (Either String a))
lookupEither msgid = do
  let key = msgIdKey msgid
  fmap eitherDecodeStrict' <$> check (get key)

lookup :: FromJSON a => MsgId -> Redis (Maybe a)
lookup msgid = do
  r <- lookupEither msgid
  return $ either (const Nothing) Just =<< r