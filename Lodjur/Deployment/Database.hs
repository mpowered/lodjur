{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Lodjur.Deployment.Database where

import           Control.Monad              (void)
import           Data.Text                  (Text)
import           Database.PostgreSQL.Simple

import           Lodjur.Database
import           Lodjur.Deployment
import           Lodjur.Git

initialize :: DbPool -> IO ()
initialize pool = withConn pool $ \conn ->
  void $ execute_ conn
    "CREATE TABLE IF NOT EXISTS deployment_job (id TEXT PRIMARY KEY, time TIMESTAMPTZ NOT NULL, deployment_name TEXT NOT NULL, tag TEXT NOT NULL, result TEXT NULL, error_message TEXT NULL)"

insertJob :: DbPool -> DeploymentJob -> Maybe JobResult -> IO ()
insertJob pool DeploymentJob {..} = \case
  Just JobSuccessful -> withConn pool $ \conn -> void $ execute
    conn
    "INSERT INTO deployment_job (id, time, deployment_name, tag, result) VALUES (?, ?, ?, ?, ?)"
    ( jobId
    , deploymentTime
    , unDeploymentName deploymentName
    , unTag deploymentTag
    , "successful" :: Text
    )
  Just (JobFailed errMsg) -> withConn pool $ \conn -> void $ execute
    conn
    "INSERT INTO deployment_job (id, time, deployment_name, tag, result, error_message) VALUES (?, ?, ?, ?, ?, ?)"
    ( jobId
    , deploymentTime
    , unDeploymentName deploymentName
    , unTag deploymentTag
    , "failed" :: Text
    , errMsg
    )
  Nothing -> withConn pool $ \conn -> void $ execute
    conn
    "INSERT INTO deployment_job (id, time, deployment_name, tag) VALUES (?, ?, ?, ?)"
    ( jobId
    , deploymentTime
    , unDeploymentName deploymentName
    , unTag deploymentTag
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
    ("successful" :: Text, errMsg, jobId)

getAllJobs :: DbPool -> IO [(DeploymentJob, Maybe JobResult)]
getAllJobs pool = withConn pool $ \conn -> mapM parseJob =<< query_
  conn
  "SELECT id, time, deployment_name, tag, result, error_message FROM deployment_job ORDER BY time DESC"
 where
  parseJob (jobId, t, name, tag, mResult, mMsg) =
    let
      job = DeploymentJob jobId (DeploymentName name) (Tag tag) t
    in
      case (mResult, mMsg) of
        (Just "failed", Just errorMessage) ->
          return (job, Just (JobFailed errorMessage))
        (Just "successful", Nothing) -> return (job, Just JobSuccessful)
        (Nothing, Nothing) -> return (job, Nothing)
        (Just result, _) -> fail ("Invalid result in database: " ++ result)
        (Nothing, Just msg) ->
          fail ("Unexpected message in database: " ++ show msg)
