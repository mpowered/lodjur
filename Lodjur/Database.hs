{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
module Lodjur.Database where

import           Control.Monad              (void)
import           Data.Pool
import           Data.Time.Clock            (NominalDiffTime)
import           Database.PostgreSQL.Simple

type DbPool = Pool Connection

newPool :: ConnectInfo -> Int -> NominalDiffTime -> Int -> IO DbPool
newPool ci = createPool (connect ci) close

destroyPool :: DbPool -> IO ()
destroyPool = destroyAllResources

withConn :: DbPool -> (Connection -> IO a) -> IO a
withConn pool a = withResource pool $ \conn -> withTransaction conn $ a conn

initialize :: DbPool -> IO ()
initialize pool = withConn pool $ \conn -> do
  void $ execute_ conn
    "CREATE TABLE IF NOT EXISTS deployment_job (id TEXT PRIMARY KEY, time TIMESTAMPTZ NOT NULL, deployment_name TEXT NOT NULL, tag TEXT NOT NULL, result TEXT NULL, error_message TEXT NULL)"
  void $ execute_ conn
    "CREATE TABLE IF NOT EXISTS event_log (time TIMESTAMPTZ NOT NULL, job_id TEXT NOT NULL, event JSONB NOT NULL)"
  void $ execute_ conn
    "CREATE TABLE IF NOT EXISTS output_log (time TIMESTAMPTZ NOT NULL, job_id TEXT NOT NULL, output TEXT NOT NULL)"

