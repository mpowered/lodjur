{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
module Lodjur.Output.Database where

import           Control.Monad              (void)
import qualified Data.HashMap.Strict        as HashMap
import           Database.PostgreSQL.Simple

import           Lodjur.Database
import           Lodjur.Output
import           Lodjur.Deployment

initialize :: DbPool -> IO ()
initialize pool = withConn pool $ \conn ->
  void $ execute_ conn
    "CREATE TABLE IF NOT EXISTS output_log (time TIMESTAMPTZ NOT NULL, job_id TEXT NOT NULL, output TEXT NOT NULL)"

appendOutput :: DbPool -> JobId -> Output -> IO ()
appendOutput pool jobid output = withConn pool $ \conn -> void $ execute
  conn
  "INSERT INTO output_log (time, job_id, output) VALUES (?, ?, ?)"
  (outputTime output, jobid, unlines (outputLines output))

getAllOutputLogs :: DbPool -> IO OutputLogs
getAllOutputLogs pool = withConn pool $ \conn -> mkOutput <$> query_
  conn
  "SELECT job_id, time, output FROM output_log ORDER BY time ASC"
 where
  mkOutput = foldr mergeOutput mempty
  mergeOutput (jobid, time, output) =
    HashMap.insertWith (++) jobid [Output time (lines output)]
