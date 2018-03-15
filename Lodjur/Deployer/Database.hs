{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Lodjur.Deployer.Database where

import           Data.Text              (Text)
import           Database.SQLite.Simple

import           Lodjur.Deployment

initialize :: Connection -> IO ()
initialize conn =
  execute_
    conn
    "CREATE TABLE IF NOT EXISTS deployment_job (id TEXT NOT NULL, deployment_name TEXT NOT NULL, tag TEXT NOT NULL, result TEXT NULL, error_message TEXT NULL)"

insertJob :: Connection -> DeploymentJob -> Maybe JobResult -> IO ()
insertJob conn DeploymentJob {..} = \case
  Just JobSuccessful -> execute
    conn
    "INSERT INTO deployment_job (id, deployment_name, tag, result) VALUES (?, ?, ?, ?)"
    ( jobId
    , unDeploymentName deploymentName
    , unTag deploymentTag
    , "successful" :: Text
    )
  Just (JobFailed errMsg) -> execute
    conn
    "INSERT INTO deployment_job (id, deployment_name, tag, result, error_message) VALUES (?, ?, ?, ?, ?)"
    ( jobId
    , unDeploymentName deploymentName
    , unTag deploymentTag
    , "failed" :: Text
    , errMsg
    )
  Nothing -> execute
    conn
    "INSERT INTO deployment_job (id, deployment_name, tag) VALUES (?, ?, ?)"
    (jobId, unDeploymentName deploymentName, unTag deploymentTag)

updateJobResult :: Connection -> JobId -> JobResult -> IO ()
updateJobResult conn jobId = \case
  JobSuccessful -> execute
    conn
    "UPDATE deployment_job SET result = ? WHERE id = ?"
    ( "successful" :: Text
    , jobId
    )
  (JobFailed errMsg) -> execute
    conn
    "UPDATE deployment_job SET result = ?, error_message = ? WHERE id = ?"
    ( "successful" :: Text
    , errMsg
    , jobId
    )

getAllJobs :: Connection -> IO [(DeploymentJob, Maybe JobResult)]
getAllJobs conn = mapM parseJob =<< query_
  conn
  "SELECT id, deployment_name, tag, result, error_message FROM deployment_job"
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
