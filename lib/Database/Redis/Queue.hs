{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}

module Database.Redis.Queue
  ( Queue
  , MsgId
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

type Queue = ByteString

data MsgId = MsgId { msgIdText :: Text, msgIdBS :: ByteString }
  deriving (Show, Eq, Ord, Generic)

instance ToJSON MsgId where
  toJSON = toJSON . msgIdText

instance FromJSON MsgId where
  parseJSON v = msgIdFromText <$> parseJSON v

data RedisError
  = RedisError Reply
  | RedisTxError String
  | RedisTxAborted
  deriving (Show, Eq)

instance Exception RedisError

msgIdFromBS :: ByteString -> MsgId
msgIdFromBS msgIdBS =
  MsgId { msgIdText = Text.decodeUtf8 msgIdBS, .. }

msgIdFromText :: Text -> MsgId
msgIdFromText msgIdText =
  MsgId { msgIdBS   = Text.encodeUtf8 msgIdText, .. }

toMsgId :: UUID -> MsgId
toMsgId a = msgIdFromText ("msg-" <> UUID.toText a)

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
  let keys' = map msgIdBS ids
  (Ok, _added) <-
    check $ multiExec $ do
      status <- mset (zip keys' (map encode' msgs))
      added <- lpush name keys'
      return $ (,) <$> status <*> added
  return ids

setTtl :: [MsgId] -> Integer -> Redis ()
setTtl msgids expiry = do
  let keys' = map msgIdBS msgids
  mapM_ (\k -> check $ expire k expiry) keys'

pop :: Queue -> Integer -> Redis (Maybe MsgId)
pop name timeout =
  fmap (msgIdFromBS . snd) <$> check (brpop [name] timeout)

poppush :: Queue -> Queue -> Integer -> Redis (Maybe MsgId)
poppush src dst timeout =
  fmap msgIdFromBS <$> check (brpoplpush src dst timeout)

move :: Queue -> Queue -> MsgId -> Redis ()
move src dst msgid = do
  let key = msgIdBS msgid
  matched <- check $ lrem src 0 key
  void $ check $ lpush dst (replicate (fromInteger matched) key)

remove :: Queue -> MsgId -> Redis ()
remove name msgid = do
  let key = msgIdBS msgid
  void $ check $
    multiExec $ do
      void $ del [key]
      lrem name 0 key

lookupEither :: FromJSON a => MsgId -> Redis (Maybe (Either String a))
lookupEither msgid = do
  let key = msgIdBS msgid
  fmap eitherDecodeStrict' <$> check (get key)

lookup :: FromJSON a => MsgId -> Redis (Maybe a)
lookup msgid = do
  r <- lookupEither msgid
  return $ either (const Nothing) Just =<< r
