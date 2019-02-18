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
-- import           Lodjur.Git
import           Lodjur.User

initialize :: DbPool -> IO ()
initialize pool = withConn pool $ \conn -> mapM_ (execute_ conn)
  [ "CREATE TABLE IF NOT EXISTS revision (id TEXT PRIMARY KEY, time TIMESTAMPTZ NOT NULL)"
  , "CREATE TABLE IF NOT EXISTS job (id TEXT PRIMARY KEY, revision_id TEXT REFERENCES revision(id), job_type TEXT NOT NULL, start_time TIMESTAMPTZ NOT NULL, finish_time TIMESTAMPTZ NULL, status TEXT NULL, started_by TEXT NOT NULL)"
  , "CREATE TABLE IF NOT EXISTS deployment (job_id TEXT REFERENCES job(id), target TEXT NOT NULL)"
  , "CREATE TABLE IF NOT EXISTS check (job_id TEXT REFERENCES job(id), application_name TEXT NOT NULL, examples INTEGER DEFAULT 0, failed INTEGER DEFAULT 0, pending INTEGER DEFAULT 0, duration REAL DEFAULT 0)"
  , "CREATE TABLE IF NOT EXISTS check_tests (job_id TEXT REFERENCES job(id), description TEXT NOT NULL, full_description TEXT NOT NULL, status TEXT NOT NULL, file_path TEXT NOT NULL, line_number INTEGER NOT NULL, exception JSONB NULL)"
  ]

insertRevision :: DbPool -> RevisionId -> UTCTime -> IO ()
insertRevision pool revision time = withConn pool $ \conn -> void $
  execute conn
    "INSERT INTO revision (id, time) VALUES (?, ?)"
    (revision, time)

insertJob :: DbPool -> Job -> IO ()
insertJob pool BuildJob {..} = withConn pool $
  insertJob' "build" buildJobId buildRevisionId buildStartTime buildFinishTime buildStatus buildStartedBy

insertJob pool DeployJob {..} = withConn pool $ \conn -> do
  insertJob' "deploy" deployJobId deployRevisionId deployStartTime deployFinishTime deployStatus deployStartedBy conn
  void $ execute conn
    "INSERT INTO deployment (job_id, target) VALUES (?, ?)"
    (deployJobId, unTarget deployTarget)

insertJob' :: Text -> JobId -> RevisionId -> UTCTime -> UTCTime -> JobStatus -> UserId -> Connection -> IO ()
insertJob' jobType jobId revisionId startTime finishTime status startedBy conn = void $
  execute conn
    "INSERT INTO build (id, revision_id, job_type, start_time, finish_time, status, started_by) VALUES (?, ?, ?, ?, ?, ?, ?)"
    (jobId, revisionId, jobType, startTime, finishTime, status, unUserId startedBy)


-- insertJob :: DbPool -> DeploymentJob -> Maybe JobResult -> IO ()
-- insertJob pool DeploymentJob {..} = \case
--   Just JobSuccessful -> withConn pool $ \conn -> void $ execute
--     conn
--     "INSERT INTO deployment_job (id, time, deployment_name, revision, started_by, result) VALUES (?, ?, ?, ?, ?, ?)"
--     ( jobId
--     , deploymentTime
--     , unDeploymentName deploymentJobName
--     , unRevision deploymentRevision
--     , unUserId deploymentJobStartedBy
--     , "successful" :: Text
--     )
--   Just (JobFailed errMsg) -> withConn pool $ \conn -> void $ execute
--     conn
--     "INSERT INTO deployment_job (id, time, deployment_name, revision, started_by, result, error_message) VALUES (?, ?, ?, ?, ?, ?, ?)"
--     ( jobId
--     , deploymentTime
--     , unDeploymentName deploymentJobName
--     , unRevision deploymentRevision
--     , unUserId deploymentJobStartedBy
--     , "failed" :: Text
--     , errMsg
--     )
--   Nothing -> withConn pool $ \conn -> void $ execute
--     conn
--     "INSERT INTO deployment_job (id, time, deployment_name, revision, started_by) VALUES (?, ?, ?, ?, ?)"
--     ( jobId
--     , deploymentTime
--     , unDeploymentName deploymentJobName
--     , unRevision deploymentRevision
--     , unUserId deploymentJobStartedBy
--     )

-- updateJobResult :: DbPool -> JobId -> JobResult -> IO ()
-- updateJobResult pool jobId = \case
--   JobSuccessful -> withConn pool $ \conn -> void $ execute
--     conn
--     "UPDATE deployment_job SET result = ? WHERE id = ?"
--     ("successful" :: Text, jobId)
--   (JobFailed errMsg) -> withConn pool $ \conn -> void $ execute
--     conn
--     "UPDATE deployment_job SET result = ?, error_message = ? WHERE id = ?"
--     ("failed" :: Text, errMsg, jobId)

-- parseJob
--   :: (Text, UTCTime, Text, Text, Maybe String, Maybe Text, Text)
--   -> IO (DeploymentJob, Maybe JobResult)
-- parseJob (jobId, t, name, revision, mResult, mMsg, userId) = do
--   let job = DeploymentJob jobId (DeploymentName name) (Revision revision) t (UserId userId)
--   case (mResult, mMsg) of
--     (Just "failed", Just errorMessage) ->
--       return (job, Just (JobFailed errorMessage))
--     (Just "successful", Nothing) -> return (job, Just JobSuccessful)
--     (Nothing, Nothing) -> return (job, Nothing)
--     (Just result, _) -> fail ("Invalid result in database: " ++ result)
--     (Nothing, Just msg) ->
--       fail ("Unexpected message in database: " ++ show msg)

-- getAllJobs :: DbPool -> Maybe Word -> IO [(DeploymentJob, Maybe JobResult)]
-- getAllJobs pool maxCount =
--   withConn pool $ \conn ->
--     mapM parseJob =<<
--     case maxCount of
--       Just n  -> query conn (baseQuery `mappend` " LIMIT ?") (Only n)
--       Nothing -> query_ conn baseQuery
--   where
--     baseQuery =
--       "SELECT id, time, deployment_name, revision, result, error_message, started_by FROM deployment_job ORDER BY time DESC"

-- getJobById :: DbPool -> JobId -> IO (Maybe (DeploymentJob, Maybe JobResult))
-- getJobById pool jobId = withConn pool $ \conn -> do
--   rows <- query
--           conn
--           "SELECT id, time, deployment_name, revision, result, error_message, started_by FROM deployment_job WHERE id = ?"
--           (Only jobId)
--   case rows of
--     []      -> return Nothing
--     (row:_) -> Just <$> parseJob row

-- insertCheckResult :: DbPool -> CheckId -> JobId -> AppName -> RSpecResult -> IO ()
-- insertCheckResult pool checkId jobId appName RSpecResult {..} = do
--   let RSpecSummary {..} = rspecSummary
--   withConn pool $ \conn -> do
--     void $ execute
--       conn
--       "INSERT INTO check_results (id, job_id, application_name, examples, failed, pending, duration) VALUES (?, ?, ?, ?, ?, ?, ?)"
--       ( checkId
--       , jobId
--       , appName
--       , rspecExampleCount
--       , rspecFailureCount
--       , rspecPendingCount
--       , rspecDuration
--       )
--     void $ executeMany
--       conn
--       "INSERT INTO check_tests (check_result_id, description, full_description, status, file_path, line_number, exception) VALUES (?, ?, ?, ?, ?, ?, ?)"
--       [ ( checkId
--         , testDescription
--         , testFullDescription
--         , testStatus
--         , testFilePath
--         , testLineNumber
--         , testException
--         )
--       | TestResult {..} <- rspecExamples
--       ]

-- getCheckResults :: DbPool -> JobId -> AppName -> IO RSpecResult
-- getCheckResults pool jobId appName = mconcat <$> getAllCheckResults pool jobId appName

-- getAllCheckResults :: DbPool -> JobId -> AppName -> IO [RSpecResult]
-- getAllCheckResults pool jobId appName = withConn pool $ \ conn -> do
--   checks <- query
--     conn
--     "SELECT id, examples, failed, pending, duration FROM check_results WHERE job_id = ? AND application_name = ?"
--     (jobId, appName)
--   forM checks $ \(checkId, rspecExampleCount, rspecFailureCount, rspecPendingCount, rspecDuration) -> do
--       let rspecSummary = RSpecSummary {..}
--       rspecExamples <- map parseTest <$> query
--         conn
--         "SELECT description, full_description, status, file_path, line_number, exception FROM check_tests WHERE check_result_id = ?"
--         (Only (checkId :: CheckId))
--       return $ RSpecResult {..}
--   where
--     parseTest (testDescription, testFullDescription, testStatus, testFilePath, testLineNumber, testException) = TestResult {..}
