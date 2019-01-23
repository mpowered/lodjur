{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Lodjur.Deployment.Database where

import           Control.Monad              (forM, void)
import           Data.Text                  (Text)
import qualified Data.Text                  as Text
import           Data.Time.Clock            (UTCTime)
import           Database.PostgreSQL.Simple

import           Lodjur.Database
import           Lodjur.Deployment
import           Lodjur.Git
import           Lodjur.User

initialize :: DbPool -> IO ()
initialize pool = withConn pool $ \conn -> mapM_ (execute_ conn)
  [ "CREATE TABLE IF NOT EXISTS deployment_job (id TEXT PRIMARY KEY, time TIMESTAMPTZ NOT NULL, deployment_name TEXT NOT NULL, revision TEXT NOT NULL, result TEXT NULL, error_message TEXT NULL, deployment_type TEXT NOT NULL, started_by TEXT NOT NULL)"
  , "CREATE INDEX IF NOT EXISTS deployment_job_time ON deployment_job (\"time\")"
  , "CREATE TABLE IF NOT EXISTS check_results (id TEXT PRIMARY KEY, job_id TEXT NOT NULL, application_name TEXT NOT NULL, examples INTEGER DEFAULT 0, failed INTEGER DEFAULT 0, pending INTEGER DEFAULT 0, duration REAL DEFAULT 0)"
  , "CREATE INDEX IF NOT EXISTS check_results_job ON check_results (\"job_id\")"
  , "CREATE TABLE IF NOT EXISTS check_tests (check_result_id TEXT NOT NULL, description TEXT NOT NULL, full_description TEXT NOT NULL, status TEXT NOT NULL, file_path TEXT NOT NULL, line_number INTEGER NOT NULL, exception JSONB NULL)"
  , "CREATE INDEX IF NOT EXISTS check_tests_check_result_id ON check_tests (\"check_result_id\")"
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

insertCheckResult :: DbPool -> CheckId -> JobId -> AppName -> RSpecResult -> IO ()
insertCheckResult pool checkId jobId appName RSpecResult {..} = do
  let RSpecSummary {..} = rspecSummary
  withConn pool $ \conn -> do
    void $ execute
      conn
      "INSERT INTO check_results (id, job_id, application_name, examples, failed, pending, duration) VALUES (?, ?, ?, ?, ?, ?, ?)"
      ( checkId
      , jobId
      , appName
      , rspecExampleCount
      , rspecFailureCount
      , rspecPendingCount
      , rspecDuration
      )
    void $ executeMany
      conn
      "INSERT INTO check_tests (check_result_id, description, full_description, status, file_path, line_number, exception) VALUES (?, ?, ?, ?, ?, ?, ?)"
      [ ( checkId
        , testDescription
        , testFullDescription
        , testStatus
        , testFilePath
        , testLineNumber
        , testException
        )
      | TestResult {..} <- rspecExamples
      ]

getCheckResults :: DbPool -> JobId -> AppName -> IO RSpecResult
getCheckResults pool jobId appName = mconcat <$> getAllCheckResults pool jobId appName

getAllCheckResults :: DbPool -> JobId -> AppName -> IO [RSpecResult]
getAllCheckResults pool jobId appName = withConn pool $ \ conn -> do
  checks <- query
    conn
    "SELECT id, examles, failed, pending, duration FROM check_results WHERE job_id = ? AND application_name = ?"
    (jobId, appName)
  forM checks $ \(checkId, rspecExampleCount, rspecFailureCount, rspecPendingCount, rspecDuration) -> do
      let rspecSummary = RSpecSummary {..}
      rspecExamples <- map parseTest <$> query
        conn
        "SELECT description, full_description, status, file_path, line_number, exception FROM check_tests WHERE check_result_id = ?"
        (Only (checkId :: CheckId))
      return $ RSpecResult {..}
  where
    parseTest (testDescription, testFullDescription, testStatus, testFilePath, testLineNumber, testException) = TestResult {..}
