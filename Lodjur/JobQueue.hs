{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Lodjur.JobQueue where

import           Control.Exception
import           Control.Monad
import           Control.Monad.Trans.Maybe
import           Database.Redis                as Redis
import           Data.Aeson                    as Aeson
import           Data.ByteString               (ByteString)
import           Data.ByteString.Lazy          (toStrict)
import           Data.Text                     (Text)
import qualified Data.Text.Encoding            as Text
import           Data.UUID                     (UUID)
import qualified Data.UUID                     as UUID
import qualified Data.UUID.V4                  as UUID
import           Prelude                       hiding (lookup)

type QueueName = ByteString

type JobId = ByteString

data RedisError
  = RedisError Reply
  | RedisTxError String
  | RedisTxAborted
  deriving (Show, Eq)

instance Exception RedisError

data Job = Job
  { githubOwner     :: !Text
  , githubRepo      :: !Text
  }
  deriving (Show, Eq)

instance ToJSON Job where
  toJSON Job {..} = object
    [ "githubOwner"     .= githubOwner
    , "githubRepo"      .= githubRepo
    ]

instance FromJSON Job where
  parseJSON = withObject "Job" $ \o -> do
    githubOwner     <- o .: "githubOwner"
    githubRepo      <- o .: "githubRepo"
    return Job {..}

toJobId :: UUID -> JobId
toJobId = Text.encodeUtf8 . UUID.toText

randomId :: IO JobId
randomId = toJobId <$> UUID.nextRandom

encode' :: ToJSON a => a -> ByteString
encode' = toStrict . encode

enqueue :: ToJSON a => Connection -> QueueName -> [a] -> IO [JobId]
enqueue conn name jobs = do
  ids <- mapM (const randomId) jobs
  added <- runRedis conn $
    multiExec $ do
      void $ hmset "jobs" (zip ids (map encode' jobs))
      mapM_ (`expire` (24*60*60)) ids
      lpush name ids
  case added of
    TxSuccess _ -> return ids
    TxAborted -> throwIO RedisTxAborted
    TxError err -> throwIO $ RedisTxError err

pop :: FromJSON a => Connection -> QueueName -> Integer -> IO (Maybe (Either String a))
pop conn name timeout = runMaybeT $ do
  jobid <- MaybeT $ popId conn name timeout
  MaybeT $ lookup conn jobid

poppush :: FromJSON a => Connection -> QueueName -> QueueName -> Integer -> IO (Maybe (Either String a))
poppush conn src dst timeout = runMaybeT $ do
  jobid <- MaybeT $ poppushId conn src dst timeout
  MaybeT $ lookup conn jobid

lookup :: FromJSON a => Connection -> JobId -> IO (Maybe (Either String a))
lookup conn jobid = do
  x <- runRedis conn $
    hget "jobs" jobid
  case x of
    Left r -> throwIO $ RedisError r
    Right Nothing -> return Nothing
    Right (Just r) -> return (Just $ eitherDecodeStrict' r)

popId :: Connection -> QueueName -> Integer -> IO (Maybe JobId)
popId conn name timeout = do
  x <- runRedis conn $
    brpop [name] timeout
  case x of
    Left r -> throwIO $ RedisError r
    Right Nothing -> return Nothing
    Right (Just (_k, jobid)) -> return (Just jobid)

poppushId :: Connection -> QueueName -> QueueName -> Integer -> IO (Maybe JobId)
poppushId conn src dst timeout = do
  x <- runRedis conn $
    brpoplpush src dst timeout
  case x of
    Left r -> throwIO $ RedisError r
    Right j -> return j
