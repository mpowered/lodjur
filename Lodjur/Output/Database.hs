{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
module Lodjur.Output.Database where

import           Control.Monad              (void)
import qualified Data.Text.Encoding         as Text
import           Data.Time.Clock
import           Database.PostgreSQL.Simple
import           Database.PostgreSQL.Simple.Notification

import           Lodjur.Database
import           Lodjur.Output

initialize :: DbPool -> IO ()
initialize pool = withConn pool $ \conn -> mapM_ (execute_ conn)
  [ "CREATE TABLE IF NOT EXISTS output_log (i BIGSERIAL, time TIMESTAMPTZ NOT NULL, log_id TEXT NOT NULL, output TEXT NOT NULL)"
  , "CREATE TABLE IF NOT EXISTS output_log_fence (i BIGINT NOT NULL, log_id TEXT NOT NULL)"
  , "CREATE INDEX IF NOT EXISTS output_log_fence_i ON output_log_fence (i)"
  , "CREATE INDEX IF NOT EXISTS output_log_fence_log_id ON output_log_fence (log_id)"
  , "CREATE INDEX IF NOT EXISTS output_log_i ON output_log (i)"
  , "CREATE INDEX IF NOT EXISTS output_log_log_id ON output_log (log_id)"
  , "CREATE INDEX IF NOT EXISTS output_log_time ON output_log (\"time\")"
  ]

appendOutput :: DbPool -> LogId -> UTCTime -> [String] -> IO ()
appendOutput pool logid time output = withConn pool $ \conn -> do
  void $ execute conn
    "INSERT INTO output_log (time, log_id, output) VALUES (?, ?, ?)"
    (time, logid, unlines output)
  notify conn logid

fence :: DbPool -> LogId -> IO ()
fence pool logid = withConn pool $ \conn -> do
  void $ execute conn
    "INSERT INTO output_log_fence (i, log_id) SELECT MAX(i), log_id FROM output_log WHERE log_id=? GROUP BY log_id"
    (Only logid)
  notify conn logid

notify :: Connection -> LogId -> IO ()
notify conn logid =
  void $ execute conn "NOTIFY output_log, ?" (Only logid)

listen :: Connection -> IO ()
listen conn =
  void $ execute_ conn "LISTEN output_log"

unlisten :: Connection -> IO ()
unlisten conn =
  void $ execute_ conn "UNLISTEN output_log"

outputNotification :: Connection -> IO (Maybe LogId)
outputNotification conn = do
  n <- getNotificationNonBlocking conn
  return $ Text.decodeUtf8 . notificationData <$> n

getOutputLog :: DbPool -> Maybe Integer -> Maybe Integer -> LogId -> IO [Output]
getOutputLog pool since before logid = withConn pool $ \conn ->
  getOutputLogConn conn since before logid

getOutputLogConn :: Connection -> Maybe Integer -> Maybe Integer -> LogId -> IO [Output]
getOutputLogConn conn since before logid = mkOutput <$>
  case (since, before) of
    (Just s, Just b) -> query conn
                "SELECT i, time, output FROM output_log WHERE log_id = ? AND i > ? AND i <= ? ORDER BY i ASC"
                (logid, s, b)
    (Just s, Nothing) -> query conn
                "SELECT i, time, output FROM output_log WHERE log_id = ? AND i > ? ORDER BY i ASC"
                (logid, s)
    (Nothing, Just b) -> query conn
                "SELECT i, time, output FROM output_log WHERE log_id = ? AND i <= ? ORDER BY i ASC"
                (logid, b)
    (Nothing, Nothing) -> query conn
                "SELECT i, time, output FROM output_log WHERE log_id = ? ORDER BY i ASC"
                (Only logid)
 where
  mkOutput = map (\(i, time, output) -> Output i time (lines output))

nextFence :: Connection -> LogId -> Maybe Integer -> IO (Maybe Integer)
nextFence conn logid since = do
  rs <- case since of
    Just s ->
      query conn
        "SELECT MIN(i) FROM output_log_fence WHERE log_id = ? AND i > ?"
        (logid, s)
    Nothing ->
      query conn
        "SELECT MIN(i) FROM output_log_fence WHERE log_id = ?"
        (Only logid)
  case rs of
    [Only i] -> return i
    _        -> return Nothing
