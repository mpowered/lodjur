{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Lodjur.JobQueue where

import           Control.Exception
import           Control.Monad
import           Database.Redis                as Redis
import           Data.Aeson                    as Aeson
import           Data.ByteString               (ByteString)
import           Data.ByteString.Lazy          (toStrict)
import           Data.Char                     (isLower)
import           Data.Text                     (Text)
import qualified Data.Text.Encoding            as Text
import           Data.UUID                     (UUID)
import qualified Data.UUID                     as UUID
import qualified Data.UUID.V4                  as UUID
import           GHC.Generics
import           Prelude                       hiding (lookup)

type QueueName = ByteString

data JobId = JobId { jobIdText :: Text, jobIdBS :: ByteString }
  deriving (Show, Eq, Ord, Generic)

instance ToJSON JobId where
  toJSON = toJSON . jobIdText

instance FromJSON JobId where
  parseJSON v = jobIdFromText <$> parseJSON v

data Conclusion = Cancelled | TimedOut | Failed | Neutral | Success
  deriving (Show, Eq, Ord, Generic)

instance ToJSON Conclusion where
  toJSON = genericToJSON jsonOptions

instance FromJSON Conclusion where
  parseJSON = genericParseJSON jsonOptions

data JobStatus = Queued | InProgress | Completed Conclusion
  deriving (Show, Eq, Ord, Generic)

instance ToJSON JobStatus where
  toJSON = genericToJSON jsonOptions

instance FromJSON JobStatus where
  parseJSON = genericParseJSON jsonOptions

data StatusChanged
  = StatusChanged
  { changedJobId    :: !JobId
  , changedStatus   :: !JobStatus
  } deriving (Show, Eq, Ord, Generic)

instance ToJSON StatusChanged where
  toJSON = genericToJSON jsonOptions

instance FromJSON StatusChanged where
  parseJSON = genericParseJSON jsonOptions

jsonOptions :: Options
jsonOptions =
  defaultOptions
    { constructorTagModifier = camelTo2 '_'
    , fieldLabelModifier = camelTo2 '_' . dropPrefix
    }
  where
    dropPrefix = dropWhile isLower

data RedisError
  = RedisError Reply
  | RedisTxError String
  | RedisTxAborted
  deriving (Show, Eq)

instance Exception RedisError

jobIdFromBS :: ByteString -> JobId
jobIdFromBS jobIdBS =
  JobId { jobIdText = Text.decodeUtf8 jobIdBS, .. }

jobIdFromText :: Text -> JobId
jobIdFromText jobIdText =
  JobId { jobIdBS   = Text.encodeUtf8 jobIdText, .. }

toJobId :: UUID -> JobId
toJobId a = jobIdFromText ("job-" <> UUID.toText a)

randomId :: IO JobId
randomId = toJobId <$> UUID.nextRandom

encode' :: ToJSON a => a -> ByteString
encode' = toStrict . encode

enqueue :: ToJSON a => Connection -> QueueName -> Integer -> [a] -> IO [JobId]
enqueue conn name expiry jobs = do
  ids <- mapM (const randomId) jobs
  let keys' = map jobIdBS ids
  added <- runRedis conn $
    multiExec $ do
      void $ mset (zip keys' (map encode' jobs))
      mapM_ (`expire` expiry) keys'
      lpush name keys'
  case added of
    TxSuccess _ -> return ids
    TxAborted -> throwIO RedisTxAborted
    TxError err -> throwIO $ RedisTxError err

setTtl :: Connection -> JobId -> Integer -> IO ()
setTtl conn jobid expiry = do
  let key = jobIdBS jobid
  x <- runRedis conn $
    expire key expiry
  case x of
    Left r -> throwIO $ RedisError r
    Right _ -> return ()

pop :: Connection -> QueueName -> Integer -> IO (Maybe JobId)
pop conn name timeout = do
  x <- runRedis conn $
    brpop [name] timeout
  case x of
    Left r -> throwIO $ RedisError r
    Right Nothing -> return Nothing
    Right (Just (_k, key)) -> return (Just $ jobIdFromBS key)

poppush :: Connection -> QueueName -> QueueName -> Integer -> IO (Maybe JobId)
poppush conn src dst timeout = do
  x <- runRedis conn $
    brpoplpush src dst timeout
  case x of
    Left r -> throwIO $ RedisError r
    Right key -> return (jobIdFromBS <$> key)

move :: Connection -> QueueName -> QueueName -> JobId -> IO ()
move conn src dst jobid = do
  let key = jobIdBS jobid
  matched <- runRedis conn $
    lrem src 0  key
  case matched of
    Left r -> throwIO $ RedisError r
    Right n -> do
      added <- runRedis conn $
        lpush dst (replicate (fromInteger n) key)
      case added of
        Left r -> throwIO $ RedisError r
        Right _ -> return ()

remove :: Connection -> QueueName -> JobId -> IO ()
remove conn name jobid = do
  let key = jobIdBS jobid
  clients <- runRedis conn $
    multiExec $ do
      void $ del [key]
      lrem name 0 key
  case clients of
    TxSuccess _ -> return ()
    TxAborted -> throwIO RedisTxAborted
    TxError err -> throwIO $ RedisTxError err

lookup :: FromJSON a => Connection -> JobId -> IO (Maybe (Either String a))
lookup conn jobid = do
  let key = jobIdBS jobid
  x <- runRedis conn $
    get key
  case x of
    Left r -> throwIO $ RedisError r
    Right Nothing -> return Nothing
    Right (Just r) -> return (Just $ eitherDecodeStrict' r)

status :: Connection -> JobId -> JobStatus -> IO ()
status conn jobid newstatus = do
  clients <- runRedis conn $
    publish "jobs_status" (encode' $ StatusChanged jobid newstatus)
  case clients of
    Left r -> throwIO $ RedisError r
    Right _ -> return ()

subscribeStatuses :: Connection -> (StatusChanged -> IO Bool) -> IO ()
subscribeStatuses conn callback =
  runRedis conn $
    pubSub (subscribe ["job_status"]) $ \msg ->
      case eitherDecodeStrict' (msgMessage msg) of
        Left _ -> return mempty
        Right change -> do
          continue <- callback change
          if continue
            then return mempty
            else return (unsubscribe ["job_status"])
