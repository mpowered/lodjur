{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
module Lodjur.Output.Database where

import           Control.Monad              (void)
import qualified Data.HashMap.Strict        as HashMap
import qualified Data.Text.Encoding         as Text
import           Data.Time.Clock
import           Database.PostgreSQL.Simple
import           Database.PostgreSQL.Simple.Notification

import           Lodjur.Database
import           Lodjur.Output
import           Lodjur.Deployment

initialize :: DbPool -> IO ()
initialize pool = withConn pool $ \conn -> do
  void $ execute_ conn
    "CREATE TABLE IF NOT EXISTS output_log (i BIGSERIAL, time TIMESTAMPTZ NOT NULL, job_id TEXT NOT NULL, output TEXT NOT NULL)"
  void $ execute_ conn
    "CREATE TABLE IF NOT EXISTS output_log_fence (i BIGINT NOT NULL, job_id TEXT NOT NULL)"

appendOutput :: DbPool -> JobId -> UTCTime -> [String] -> IO ()
appendOutput pool jobid time output = withConn pool $ \conn -> do
  void $ execute conn
    "INSERT INTO output_log (time, job_id, output) VALUES (?, ?, ?)"
    (time, jobid, unlines output)
  notify conn jobid

fence :: DbPool -> JobId -> IO ()
fence pool jobid = withConn pool $ \conn -> do
  void $ execute conn
    "INSERT INTO output_log_fence (i, job_id) SELECT MAX(i), job_id FROM output_log WHERE job_id=? GROUP BY job_id"
    (Only jobid)
  notify conn jobid

notify :: Connection -> JobId -> IO ()
notify conn jobid =
  void $ execute conn "NOTIFY output_log, ?" (Only jobid)

listen :: Connection -> IO ()
listen conn =
  void $ execute_ conn "LISTEN output_log"

unlisten :: Connection -> IO ()
unlisten conn =
  void $ execute_ conn "UNLISTEN output_log"

outputNotification :: Connection -> IO (Maybe JobId)
outputNotification conn = do
  n <- getNotificationNonBlocking conn
  return $ Text.decodeUtf8 . notificationData <$> n

getOutputLog :: DbPool -> Maybe Integer -> Maybe Integer -> JobId -> IO [Output]
getOutputLog pool since before jobid = withConn pool $ \conn ->
  getOutputLogConn conn since before jobid

getOutputLogConn :: Connection -> Maybe Integer -> Maybe Integer -> JobId -> IO [Output]
getOutputLogConn conn since before jobid = mkOutput <$>
  case (since, before) of
    (Just s, Just b) -> query conn
                "SELECT i, time, output FROM output_log WHERE job_id = ? AND time > ? AND time <= ? ORDER BY i ASC"
                (jobid, s, b)
    (Just s, Nothing) -> query conn
                "SELECT i, time, output FROM output_log WHERE job_id = ? AND time > ? ORDER BY i ASC"
                (jobid, s)
    (Nothing, Just b) -> query conn
                "SELECT i, time, output FROM output_log WHERE job_id = ? AND time <= ? ORDER BY i ASC"
                (jobid, b)
    (Nothing, Nothing) -> query conn
                "SELECT i, time, output FROM output_log WHERE job_id = ? ORDER BY i ASC"
                (Only jobid)
 where
  mkOutput = map (\(i, time, output) -> Output i time (lines output))

nextFence :: Connection -> JobId -> Maybe Integer -> IO (Maybe Integer)
nextFence conn jobid since = do
  rs <- case since of
    Just s ->
      query conn
        "SELECT MIN(i) FROM output_log_fence WHERE job_id = ? AND i > ?"
        (jobid, s)
    Nothing ->
      query conn
        "SELECT MIN(i) FROM output_log_fence WHERE job_id = ?"
        (Only jobid)
  case rs of
    [Only i] -> return i
    _        -> return Nothing

getAllOutputLogs :: DbPool -> IO OutputLogs
getAllOutputLogs pool = withConn pool $ \conn -> mkOutput <$> query_
  conn
  "SELECT job_id, i, time, output FROM output_log ORDER BY time ASC"
 where
  mkOutput = foldr mergeOutput mempty
  mergeOutput (i, jobid, time, output) =
    HashMap.insertWith (++) jobid [Output i time (lines output)]
