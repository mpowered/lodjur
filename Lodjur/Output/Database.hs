{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
module Lodjur.Output.Database where

import           Control.Monad              (void)
import qualified Data.Text.Encoding         as Text
import           Data.Time.Clock
import           Lodjur.Deployment          (JobId)
import           Database.PostgreSQL.Simple
import           Database.PostgreSQL.Simple.Notification

import           Lodjur.Database
import           Lodjur.Output

initialize :: DbPool -> IO ()
initialize pool = withConn pool $ \conn -> mapM_ (execute_ conn)
  [ "CREATE TABLE IF NOT EXISTS output_log (i BIGSERIAL, time TIMESTAMPTZ NOT NULL, job_id TEXT NOT NULL, output TEXT NOT NULL)"
  , "CREATE TABLE IF NOT EXISTS output_log_fence (i BIGINT NOT NULL, job_id TEXT NOT NULL)"
  , "CREATE INDEX IF NOT EXISTS output_log_fence_i ON output_log_fence (i)"
  , "CREATE INDEX IF NOT EXISTS output_log_fence_job_id ON output_log_fence (job_id)"
  , "CREATE INDEX IF NOT EXISTS output_log_i ON output_log (i)"
  , "CREATE INDEX IF NOT EXISTS output_log_job_id ON output_log (job_id)"
  , "CREATE INDEX IF NOT EXISTS output_log_time ON output_log (\"time\")"
  ]

appendOutput :: DbPool -> JobId -> UTCTime -> [String] -> IO ()
appendOutput pool jobId time output = withConn pool $ \conn -> do
  void $ execute conn
    "INSERT INTO output_log (time, job_id, output) VALUES (?, ?, ?)"
    (time, jobId, unlines output)
  notify conn jobId

fence :: DbPool -> JobId -> IO ()
fence pool jobId = withConn pool $ \conn -> do
  void $ execute conn
    "INSERT INTO output_log_fence (i, job_id) SELECT MAX(i), job_id FROM output_log WHERE job_id=? GROUP BY job_id"
    (Only jobId)
  notify conn jobId

notify :: Connection -> JobId -> IO ()
notify conn jobId =
  void $ execute conn "NOTIFY output_log, ?" (Only jobId)

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
getOutputLog pool since before jobId = withConn pool $ \conn ->
  getOutputLogConn conn since before jobId

getOutputLogConn :: Connection -> Maybe Integer -> Maybe Integer -> JobId -> IO [Output]
getOutputLogConn conn since before jobId = mkOutput <$>
  case (since, before) of
    (Just s, Just b) -> query conn
                "SELECT i, time, output FROM output_log WHERE job_id = ? AND i > ? AND i <= ? ORDER BY i ASC"
                (jobId, s, b)
    (Just s, Nothing) -> query conn
                "SELECT i, time, output FROM output_log WHERE job_id = ? AND i > ? ORDER BY i ASC"
                (jobId, s)
    (Nothing, Just b) -> query conn
                "SELECT i, time, output FROM output_log WHERE job_id = ? AND i <= ? ORDER BY i ASC"
                (jobId, b)
    (Nothing, Nothing) -> query conn
                "SELECT i, time, output FROM output_log WHERE job_id = ? ORDER BY i ASC"
                (Only jobId)
 where
  mkOutput = map (\(i, time, output) -> Output i time (lines output))

nextFence :: Connection -> JobId -> Maybe Integer -> IO (Maybe Integer)
nextFence conn jobId since = do
  rs <- case since of
    Just s ->
      query conn
        "SELECT MIN(i) FROM output_log_fence WHERE job_id = ? AND i > ?"
        (jobId, s)
    Nothing ->
      query conn
        "SELECT MIN(i) FROM output_log_fence WHERE job_id = ?"
        (Only jobId)
  case rs of
    [Only i] -> return i
    _        -> return Nothing
