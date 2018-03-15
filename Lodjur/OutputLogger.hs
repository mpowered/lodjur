{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeFamilies               #-}
module Lodjur.OutputLogger
  ( Output
  , OutputLogs
  , OutputLogger
  , newOutputLogger
  , OutputLogMessage (..)
  ) where

import           Control.Concurrent
import           Control.Exception
import           Data.Time.Clock
import           Data.HashMap.Strict    (HashMap)
import qualified Data.HashMap.Strict    as HashMap
import           Database.SQLite.Simple

import           Lodjur.Deployment
import           Lodjur.Process

newtype OutputLogger = OutputLogger Connection

type Output = [String]

type OutputLogs = HashMap JobId Output

retryIfBusy :: IO a -> IO a
retryIfBusy a = do
  e <- tryJust isBusy a
  case e of
    Left _ -> threadDelay 10 >> retryIfBusy a
    Right r -> return r
 where
  isBusy sqlerr
    | sqlError sqlerr == ErrorBusy = Just ()
    | otherwise                     = Nothing

newOutputLogger :: String -> IO OutputLogger
newOutputLogger file = OutputLogger <$> openAndInit file

openAndInit :: String -> IO Connection
openAndInit file = do
    conn <- open file
    retryIfBusy $
      execute_ conn "CREATE TABLE IF NOT EXISTS job_output_log (time TEXT, job_id TEXT, output TEXT)"
    return conn

appendOutput :: Connection -> UTCTime -> JobId -> Output -> IO ()
appendOutput conn t jobid output = retryIfBusy $
  execute conn "INSERT INTO job_output_log (time, job_id, output) VALUES (?, ?, ?)"
    (t, jobid, unlines output)

getAllOutputLogs :: Connection -> IO OutputLogs
getAllOutputLogs conn = retryIfBusy $
  mkOutput <$> query_ conn "SELECT job_id, output FROM job_output_log ORDER BY time ASC"
 where
  mkOutput = foldr mergeOutput mempty
  mergeOutput (jobid, output) = HashMap.insertWith (++) jobid (lines output)

data OutputLogMessage r where
  -- Public messages:
  AppendOutput :: JobId -> Output -> OutputLogMessage Async
  GetOutputLogs :: OutputLogMessage (Sync OutputLogs)

instance Process OutputLogger where
  type Message OutputLogger = OutputLogMessage

  receive _self (a@(OutputLogger conn), AppendOutput jobid output) = do
    now <- getCurrentTime
    appendOutput conn now jobid output
    return a

  receive _self (a@(OutputLogger conn), GetOutputLogs) = do
    logs <- getAllOutputLogs conn
    return (a, logs)

  terminate (OutputLogger conn) = close conn
