{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Lodjur.Deployment.Database where

import           Control.Monad              (void)
import           Data.Text                  (Text)
import           Data.Time.Clock            (UTCTime)
import           Database.PostgreSQL.Simple

import           Lodjur.Database
import           Lodjur.Deployment
import           Lodjur.Git
import           Lodjur.User

initialize :: DbPool -> IO ()
initialize pool = withConn pool $ \conn -> mapM_ (execute_ conn)
  [ "CREATE TABLE IF NOT EXISTS deployment_job (id TEXT PRIMARY KEY, time TIMESTAMPTZ NOT NULL, deployment_name TEXT NOT NULL, revision TEXT NOT NULL, result TEXT NULL, error_message TEXT NULL, build_only BOOLEAN NOT NULL, started_by TEXT NOT NULL)"
  , "CREATE INDEX IF NOT EXISTS deployment_job_time ON deployment_job (\"time\")"
  ]

insertJob :: DbPool -> DeploymentJob -> Maybe JobResult -> IO ()
insertJob pool DeploymentJob {..} = \case
  Just JobSuccessful -> withConn pool $ \conn -> void $ execute
    conn
    "INSERT INTO deployment_job (id, time, deployment_name, revision, build_only, started_by, result) VALUES (?, ?, ?, ?, ?, ?, ?)"
    ( jobId
    , deploymentTime
    , unDeploymentName deploymentJobName
    , unRevision deploymentRevision
    , deploymentBuildOnly
    , unUserId deploymentJobStartedBy
    , "successful" :: Text
    )
  Just (JobFailed errMsg) -> withConn pool $ \conn -> void $ execute
    conn
    "INSERT INTO deployment_job (id, time, deployment_name, revision, build_only, started_by, result, error_message) VALUES (?, ?, ?, ?, ?, ?, ?, ?)"
    ( jobId
    , deploymentTime
    , unDeploymentName deploymentJobName
    , unRevision deploymentRevision
    , deploymentBuildOnly
    , unUserId deploymentJobStartedBy
    , "failed" :: Text
    , errMsg
    )
  Nothing -> withConn pool $ \conn -> void $ execute
    conn
    "INSERT INTO deployment_job (id, time, deployment_name, revision, build_only, started_by) VALUES (?, ?, ?, ?, ?, ?)"
    ( jobId
    , deploymentTime
    , unDeploymentName deploymentJobName
    , unRevision deploymentRevision
    , deploymentBuildOnly
    , unUserId deploymentJobStartedBy
    )

updateJobResult :: DbPool -> JobId -> JobResult -> IO ()
updateJobResult pool jobId = \case
  JobSuccessful -> withConn pool $ \conn -> void $ execute
    conn
    "UPDATE deployment_job SET result = ? WHERE id = ?"
    ("successful" :: Text, jobId)
  (JobFailed errMsg) -> withConn pool $ \conn -> void $ execute
    conn
    "UPDATE deployment_job SET result = ?, error_message = ? WHERE id = ?"
    ("failed" :: Text, errMsg, jobId)

parseJob
  :: (Text, UTCTime, Text, Text, Maybe String, Maybe Text, Bool, Text)
  -> IO (DeploymentJob, Maybe JobResult)
parseJob (jobId, t, name, revision, mResult, mMsg, buildOnly, userId) =
  let
    job = DeploymentJob jobId (DeploymentName name) (Revision revision) t buildOnly (UserId userId)
  in
    case (mResult, mMsg) of
      (Just "failed", Just errorMessage) ->
        return (job, Just (JobFailed errorMessage))
      (Just "successful", Nothing) -> return (job, Just JobSuccessful)
      (Nothing, Nothing) -> return (job, Nothing)
      (Just result, _) -> fail ("Invalid result in database: " ++ result)
      (Nothing, Just msg) ->
        fail ("Unexpected message in database: " ++ show msg)

getAllJobs :: DbPool -> Maybe Word -> IO [(DeploymentJob, Maybe JobResult)]
getAllJobs pool maxCount =
  withConn pool $ \conn ->
    mapM parseJob =<<
    case maxCount of
      Just n  -> query conn (baseQuery `mappend` " LIMIT ?") (Only n)
      Nothing -> query_ conn baseQuery
  where
    baseQuery =
      "SELECT id, time, deployment_name, revision, result, error_message, build_only, started_by FROM deployment_job ORDER BY time DESC"

getJobById :: DbPool -> JobId -> IO (Maybe (DeploymentJob, Maybe JobResult))
getJobById pool jobId = withConn pool $ \conn -> do
  rows <- query
          conn
          "SELECT id, time, deployment_name, revision, result, error_message, build_only, started_by FROM deployment_job WHERE id = ?"
          [jobId]
  case rows of
    []      -> return Nothing
    (row:_) -> Just <$> parseJob row
