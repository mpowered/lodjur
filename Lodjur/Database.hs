{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE ViewPatterns      #-}
module Lodjur.Database where

import           Control.Concurrent     (threadDelay)
import           Control.Exception      (Exception, throwIO, tryJust)
import           Control.Monad          (foldM)
import           Data.Aeson
import qualified Data.HashMap.Strict    as HashMap
import           Data.Text              (Text)
import           Data.Time.Clock        (UTCTime)
import           Database.SQLite.Simple

import           Lodjur.Deployment

newtype EventDecodeFailed = EventDecodeFailed String
  deriving (Eq, Show)

instance Exception EventDecodeFailed

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

initialize :: Connection -> IO ()
initialize conn = retryIfBusy $ do
  execute_
    conn
    "CREATE TABLE IF NOT EXISTS deployment_job (id TEXT NOT NULL, deployment_name TEXT NOT NULL, tag TEXT NOT NULL, result TEXT NULL, error_message TEXT NULL)"
  execute_
    conn
    "CREATE TABLE IF NOT EXISTS job_event_log (time TEXT, job_id TEXT, event TEXT)"
  execute_
    conn
    "CREATE TABLE IF NOT EXISTS job_event_log (time TEXT, job_id TEXT, event TEXT)"

insertJob :: Connection -> DeploymentJob -> Maybe JobResult -> IO ()
insertJob conn DeploymentJob {..} = \case
  Just JobSuccessful -> retryIfBusy $ execute
    conn
    "INSERT INTO deployment_job (id, deployment_name, tag, result) VALUES (?, ?, ?, ?)"
    ( jobId
    , unDeploymentName deploymentName
    , unTag deploymentTag
    , "successful" :: Text
    )
  Just (JobFailed errMsg) -> retryIfBusy $ execute
    conn
    "INSERT INTO deployment_job (id, deployment_name, tag, result, error_message) VALUES (?, ?, ?, ?, ?)"
    ( jobId
    , unDeploymentName deploymentName
    , unTag deploymentTag
    , "failed" :: Text
    , errMsg
    )
  Nothing -> retryIfBusy $ execute
    conn
    "INSERT INTO deployment_job (id, deployment_name, tag) VALUES (?, ?, ?)"
    (jobId, unDeploymentName deploymentName, unTag deploymentTag)

updateJobResult :: Connection -> JobId -> JobResult -> IO ()
updateJobResult conn jobId = \case
  JobSuccessful -> retryIfBusy $ execute
    conn
    "UPDATE deployment_job SET result = ? WHERE id = ?"
    ( "successful" :: Text
    , jobId
    )
  (JobFailed errMsg) -> retryIfBusy $ execute
    conn
    "UPDATE deployment_job SET result = ?, error_message = ? WHERE id = ?"
    ( "successful" :: Text
    , errMsg
    , jobId
    )

getAllJobs :: Connection -> IO [(DeploymentJob, Maybe JobResult)]
getAllJobs conn = mapM parseJob =<< retryIfBusy (query_
  conn
  "SELECT id, deployment_name, tag, result, error_message FROM deployment_job")
 where
  parseJob (jobId, name, tag, mResult, mMsg) =
    let
      job = DeploymentJob jobId (DeploymentName name) (Tag tag)
    in
      case (mResult, mMsg) of
        (Just "failed", Just errorMessage) ->
          return (job, Just (JobFailed errorMessage))
        (Just "successful", Nothing) -> return (job, Just JobSuccessful)
        (Nothing          , Nothing) -> return (job, Nothing)
        (Just result, _) ->
          fail ("Invalid result in database: " ++ result)
        (Nothing, Just msg) ->
          fail ("Unexpected message in database: " ++ show msg)

insertEvent :: ToJSON event => Connection -> UTCTime -> JobId -> event -> IO ()
insertEvent conn t jobid event = retryIfBusy $ execute
  conn
  "INSERT INTO job_event_log (time, job_id, event) VALUES (?, ?, ?)"
  (t, jobid, encode event)

getAllEventLogs :: Connection -> IO EventLogs
getAllEventLogs conn = retryIfBusy $ mkEventLog
  =<< query_ conn "SELECT job_id, event FROM job_event_log ORDER BY time ASC"
 where
  mkEventLog = foldM mergeEvent mempty
  mergeEvent m (jobid, eitherDecode -> event) = case event of
    Left  msg -> throwIO $ EventDecodeFailed msg
    Right e   -> return $ HashMap.insertWith (++) jobid [e] m

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
