{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
module Lodjur.Output.Database where

import           Control.Concurrent.BoundedChan
import           Control.Monad              (void, unless, when)
import qualified Data.HashMap.Strict        as HashMap
import           Data.Maybe                 (isNothing)
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
    "CREATE TABLE IF NOT EXISTS output_log (time TIMESTAMPTZ NOT NULL, job_id TEXT NOT NULL, output TEXT NOT NULL)"
  void $ execute_ conn
    "CREATE TABLE IF NOT EXISTS output_log_fence (time TIMESTAMPTZ NOT NULL, job_id TEXT NOT NULL)"

appendOutput :: DbPool -> JobId -> Output -> IO ()
appendOutput pool jobid output = withConn pool $ \conn -> do
  void $ execute conn
    "INSERT INTO output_log (time, job_id, output) VALUES (?, ?, ?)"
    (outputTime output, jobid, unlines (outputLines output))
  notify conn jobid

fence :: DbPool -> JobId -> IO ()
fence pool jobid = withConn pool $ \conn -> do
  void $ execute conn
    "INSERT INTO output_log_fence (time, job_id) SELECT MAX(time), job_id FROM output_log WHERE job_id=? GROUP BY job_id"
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

waitJobNotification :: Connection -> JobId -> IO ()
waitJobNotification conn jobid = do
  n <- getNotification conn
  unless (Text.encodeUtf8 jobid == notificationData n) $
    waitJobNotification conn jobid

getOutputLog :: DbPool -> Maybe UTCTime -> Maybe UTCTime -> JobId -> IO [Output]
getOutputLog pool since before jobid = withConn pool $ \conn -> mkOutput <$>
  case (since, before) of
    (Just s, Just b) -> query conn
                "SELECT time, output FROM output_log WHERE job_id = ? AND time > ? AND time <= ? ORDER BY time ASC"
                (jobid, s, b)
    (Just s, Nothing) -> query conn
                "SELECT time, output FROM output_log WHERE job_id = ? AND time > ? ORDER BY time ASC"
                (jobid, s)
    (Nothing, Just b) -> query conn
                "SELECT time, output FROM output_log WHERE job_id = ? AND time <= ? ORDER BY time ASC"
                (jobid, b)
    (Nothing, Nothing) -> query conn
                "SELECT time, output FROM output_log WHERE job_id = ? ORDER BY time ASC"
                (Only jobid)
 where
  mkOutput = map (\(time, output) -> Output time (lines output))

streamOutputLog :: DbPool -> JobId -> Maybe UTCTime -> BoundedChan (Maybe Output) -> IO ()
streamOutputLog pool jobid s chan = withConnNoTran pool $ \conn -> do
  listen conn
  go conn s Nothing
  unlisten conn
  writeChan chan Nothing
 where
  go conn since til = do
    til' <- maybe (nextFence conn) (return . Just) til
    output <- getOutputLog pool since til' jobid
    writeList2Chan chan (map Just output)
    when (isNothing til') $ do
        waitJobNotification conn jobid
        go conn (lastSeen since output) Nothing

  lastSeen since [] = since
  lastSeen _     os = Just $ outputTime $ last os

  nextFence conn = do
    rs <- query
            conn
            "SELECT MIN(time) FROM output_log_fence WHERE job_id = ?"
            (Only jobid)
    case rs of
      [Only mt] -> return mt
      _         -> return Nothing

getAllOutputLogs :: DbPool -> IO OutputLogs
getAllOutputLogs pool = withConn pool $ \conn -> mkOutput <$> query_
  conn
  "SELECT job_id, time, output FROM output_log ORDER BY time ASC"
 where
  mkOutput = foldr mergeOutput mempty
  mergeOutput (jobid, time, output) =
    HashMap.insertWith (++) jobid [Output time (lines output)]
