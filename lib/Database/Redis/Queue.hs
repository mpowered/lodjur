{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Database.Redis.Queue
  ( Queue (..)
  , push
  , pop
  , pop'
  , poppush
  , poppush'
  , move
  , remove
  )
where

import           Control.Monad
import           Database.Redis                hiding (move)
import           Database.Redis.Exception
import           Data.Aeson                    as Aeson
import           Data.ByteString               (ByteString)
import           Data.ByteString.Lazy          (toStrict)
import           Data.Text                     (Text)
import qualified Data.Text.Encoding            as Text
import           GHC.Generics
import           Prelude                       hiding (lookup)

newtype Queue a = Queue { unQueue :: Text }
  deriving (Show, Eq, Ord, Generic)

queueKey :: Queue a -> ByteString
queueKey = Text.encodeUtf8 . unQueue

encode' :: ToJSON a => a -> ByteString
encode' = toStrict . encode

push :: ToJSON a => Queue a -> [a] -> Redis ()
push q xs =
  void $ check $ lpush (queueKey q) (map encode' xs)

pop :: FromJSON a => [Queue a] -> Integer -> Redis (Maybe (Queue a, a))
pop qs timeout = do
  r <- pop' qs timeout
  return $ case r of
    Nothing -> Nothing
    Just (q, e) ->
      case e of
        Left _ -> Nothing
        Right a -> Just (q, a)

pop' :: FromJSON a => [Queue a] -> Integer -> Redis (Maybe (Queue a, Either String a))
pop' qs timeout = do
  r <- check $ brpop (map queueKey qs) timeout
  return $ fmap go r
  where
    go (qk, bs) = (Queue $ Text.decodeUtf8 qk, eitherDecodeStrict bs)

poppush :: FromJSON a => Queue a -> Queue a -> Integer -> Redis (Maybe a)
poppush src dst timeout = do
  r <- poppush' src dst timeout
  return $ case r of
    Nothing -> Nothing
    Just e ->
      case e of
        Left _ -> Nothing
        Right a -> Just a

poppush' :: FromJSON a => Queue a -> Queue a -> Integer -> Redis (Maybe (Either String a))
poppush' src dst timeout = do
  r <- check $ brpoplpush (queueKey src) (queueKey dst) timeout
  return $ fmap eitherDecodeStrict r

move :: ToJSON a => Queue a -> Queue a -> a -> Redis Integer
move src dst x = do
  let x' = encode' x
  matched <- check $ lrem (queueKey src) 0 x'
  void $ check $ lpush (queueKey dst) (replicate (fromInteger matched) x')
  return matched

remove :: ToJSON a => Queue a -> a -> Redis Integer
remove q x = do
  let x' = encode' x
  check $ lrem (queueKey q) 0 x'
