{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Lodjur.Deployment.Database where

import           Control.Monad              (void)
import           Data.Text                  (Text)
import           Data.Text                  as Text
import           Data.Time.Clock            (UTCTime)
import qualified Data.UUID                  as UUID
import qualified Data.UUID.V4               as UUID
import           Database.PostgreSQL.Simple

import           Lodjur.Database
import           Lodjur.Deployment
import           Lodjur.Git
import           Lodjur.Output
import           Lodjur.User

initialize :: DbPool -> IO ()
initialize pool = withConn pool $ \conn -> mapM_ (execute_ conn)
  [ "CREATE TABLE IF NOT EXISTS deployment_job (id TEXT PRIMARY KEY, time TIMESTAMPTZ NOT NULL, deployment_name TEXT NOT NULL, revision TEXT NOT NULL, result TEXT NULL, error_message TEXT NULL, deployment_type TEXT NOT NULL, started_by TEXT NOT NULL)"
  , "CREATE INDEX IF NOT EXISTS deployment_job_time ON deployment_job (\"time\")"
  , "CREATE TABLE IF NOT EXISTS deployment_log (job_id TEXT, log_type TEXT, log_id TEXT)"
  , "CREATE INDEX IF NOT EXISTS deployment_log_job_logty ON deployment_log (\"job_id\", \"log_type\")"
  ]

insertJob :: DbPool -> DeploymentJob -> Maybe JobResult -> IO ()
insertJob pool DeploymentJob {..} = \case
  Just JobSuccessful -> withConn pool $ \conn -> void $ execute
    conn
    "INSERT INTO deployment_job (id, time, deployment_name, revision, deployment_type, started_by, result) VALUES (?, ?, ?, ?, ?, ?, ?)"
    ( jobId
    , deploymentTime
    , unDeploymentName deploymentJobName
    , unRevision deploymentRevision
    , deployTypeText deploymentType
    , unUserId deploymentJobStartedBy
    , "successful" :: Text
    )
  Just (JobFailed errMsg) -> withConn pool $ \conn -> void $ execute
    conn
    "INSERT INTO deployment_job (id, time, deployment_name, revision, deployment_type, started_by, result, error_message) VALUES (?, ?, ?, ?, ?, ?, ?, ?)"
    ( jobId
    , deploymentTime
    , unDeploymentName deploymentJobName
    , unRevision deploymentRevision
    , deployTypeText deploymentType
    , unUserId deploymentJobStartedBy
    , "failed" :: Text
    , errMsg
    )
  Nothing -> withConn pool $ \conn -> void $ execute
    conn
    "INSERT INTO deployment_job (id, time, deployment_name, revision, deployment_type, started_by) VALUES (?, ?, ?, ?, ?, ?)"
    ( jobId
    , deploymentTime
    , unDeploymentName deploymentJobName
    , unRevision deploymentRevision
    , deployTypeText deploymentType
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
  :: (Text, UTCTime, Text, Text, Maybe String, Maybe Text, Text, Text)
  -> IO (DeploymentJob, Maybe JobResult)
parseJob (jobId, t, name, revision, mResult, mMsg, txtDeployType, userId) = do
  deployType <-
    maybe
      (fail ("Invalid deploy type in database: " ++ Text.unpack txtDeployType))
      return
      (parseDeployType txtDeployType)
  let job = DeploymentJob jobId (DeploymentName name) (Revision revision) t deployType (UserId userId)
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
      "SELECT id, time, deployment_name, revision, result, error_message, deployment_type, started_by FROM deployment_job ORDER BY time DESC"

getJobById :: DbPool -> JobId -> IO (Maybe (DeploymentJob, Maybe JobResult))
getJobById pool jobId = withConn pool $ \conn -> do
  rows <- query
          conn
          "SELECT id, time, deployment_name, revision, result, error_message, deployment_type, started_by FROM deployment_job WHERE id = ?"
          (Only jobId)
  case rows of
    []      -> return Nothing
    (row:_) -> Just <$> parseJob row

deployTypeText :: DeploymentType -> Text
deployTypeText BuildOnly   = "build"
deployTypeText BuildCheck  = "check"
deployTypeText BuildDeploy = "deploy"

parseDeployType :: Text -> Maybe DeploymentType
parseDeployType "build"  = Just BuildOnly
parseDeployType "check"  = Just BuildCheck
parseDeployType "deploy" = Just BuildDeploy
parseDeployType _        = Nothing

createJobLog :: DbPool -> JobId -> LogType -> IO LogId
createJobLog pool jobid logty = do
  logid <- UUID.toText <$> UUID.nextRandom
  associateJobLog pool jobid logty logid
  return logid

associateJobLog :: DbPool -> JobId -> LogType -> LogId -> IO ()
associateJobLog pool jobid logty logid =
  withConn pool $ \conn -> void $ execute
    conn
    "INSERT INTO deployment_log (job_id, log_type, log_id) VALUES (?, ?, ?)"
    ( jobid, logty, logid )

getJobLog :: DbPool -> JobId -> LogType -> IO (Maybe LogId)
getJobLog pool jobid logty = withConn pool $ \conn -> do
  rows <- query
    conn
    "SELECT log_id FROM deployment_log WHERE job_id = ? AND log_type = ?"
    (jobid, logty)
  case rows of
    []      -> return Nothing
    (Only logid:_) -> return $ Just logid

getJobLogs :: DbPool -> JobId -> IO [(LogType, LogId)]
getJobLogs pool jobid = withConn pool $ \conn ->
  query
    conn
    "SELECT log_type, log_id FROM deployment_log WHERE job_id = ?"
    (Only jobid)
