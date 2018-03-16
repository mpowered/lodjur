{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE ViewPatterns      #-}
module Lodjur.Database where

import           Control.Exception           (throwIO)
import           Control.Monad               (foldM, void)
import           Data.Aeson
import qualified Data.HashMap.Strict         as HashMap
import           Data.Pool
import           Data.Text                   (Text)
import           Data.Time.Clock             (NominalDiffTime, UTCTime)
import           Database.PostgreSQL.Simple

import           Lodjur.Deployment

type DbPool = Pool Connection

newPool :: ConnectInfo -> Int -> NominalDiffTime -> Int -> IO DbPool
newPool ci = createPool (connect ci) close

destroyPool :: DbPool -> IO ()
destroyPool = destroyAllResources

withConn :: DbPool -> (Connection -> IO a) -> IO a
withConn pool a =
  withResource pool $ \conn ->
    withTransaction conn $
      a conn

initialize :: DbPool -> IO ()
initialize pool = withConn pool $ \conn -> do
  void $ execute_ conn
    "CREATE TABLE IF NOT EXISTS deployment_job (id TEXT PRIMARY KEY, time TIMESTAMP NOT NULL, deployment_name TEXT NOT NULL, tag TEXT NOT NULL, result TEXT NULL, error_message TEXT NULL)"
  void $ execute_ conn
    "CREATE TABLE IF NOT EXISTS event_log (time TIMESTAMP NOT NULL, job_id TEXT NOT NULL REFERENCES deployment_job(id) ON DELETE CASCADE ON UPDATE CASCADE, event JSONB NOT NULL)"
  void $ execute_ conn
    "CREATE TABLE IF NOT EXISTS output_log (time TIMESTAMP NOT NULL, job_id TEXT NOT NULL REFERENCES deployment_job(id) ON DELETE CASCADE ON UPDATE CASCADE, output TEXT NOT NULL)"


insertJob :: DbPool -> DeploymentJob -> Maybe JobResult -> IO ()
insertJob pool DeploymentJob {..} = \case
  Just JobSuccessful -> withConn pool $ \conn ->
    void $ execute conn
    "INSERT INTO deployment_job (id, time, deployment_name, tag, result) VALUES (?, ?, ?, ?)"
    ( jobId
    , deploymentTime
    , unDeploymentName deploymentName
    , unTag deploymentTag
    , "successful" :: Text
    )
  Just (JobFailed errMsg) -> withConn pool $ \conn ->
    void $ execute conn
    "INSERT INTO deployment_job (id, time, deployment_name, tag, result, error_message) VALUES (?, ?, ?, ?, ?)"
    ( jobId
    , deploymentTime
    , unDeploymentName deploymentName
    , unTag deploymentTag
    , "failed" :: Text
    , errMsg
    )
  Nothing -> withConn pool $ \conn ->
    void $ execute conn
    "INSERT INTO deployment_job (id, time, deployment_name, tag) VALUES (?, ?, ?)"
    ( jobId
    , deploymentTime
    , unDeploymentName deploymentName
    , unTag deploymentTag
    )

updateJobResult :: DbPool -> JobId -> JobResult -> IO ()
updateJobResult pool jobId = \case
  JobSuccessful -> withConn pool $ \conn ->
    void $ execute conn
    "UPDATE deployment_job SET result = ? WHERE id = ?"
    ( "successful" :: Text
    , jobId
    )
  (JobFailed errMsg) -> withConn pool $ \conn ->
    void $ execute conn
    "UPDATE deployment_job SET result = ?, error_message = ? WHERE id = ?"
    ( "successful" :: Text
    , errMsg
    , jobId
    )

getAllJobs :: DbPool -> IO [(DeploymentJob, Maybe JobResult)]
getAllJobs pool = withConn pool $ \conn ->
  mapM parseJob =<< query_ conn
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
        (Just result, _) ->
          fail ("Invalid result in database: " ++ result)
        (Nothing, Just msg) ->
          fail ("Unexpected message in database: " ++ show msg)

insertEvent :: DbPool -> UTCTime -> JobId -> JobEvent -> IO ()
insertEvent pool t jobid event = withConn pool $ \conn ->
  void $ execute conn
  "INSERT INTO event_log (time, job_id, event) VALUES (?, ?, ?)"
  (t, jobid, toJSON event)

getAllEventLogs :: DbPool -> IO EventLogs
getAllEventLogs pool = withConn pool $ \conn ->
  mkEventLog =<< query_ conn
  "SELECT job_id, event FROM event_log ORDER BY time ASC"
 where
  mkEventLog = foldM mergeEvent mempty
  mergeEvent m (jobid, fromJSON -> event) = case event of
    Error  msg -> throwIO $ EventDecodeFailed msg
    Success e  -> return $ HashMap.insertWith (++) jobid [e] m

appendOutput :: DbPool -> UTCTime -> JobId -> Output -> IO ()
appendOutput pool t jobid output = withConn pool $ \conn ->
  void $ execute conn
  "INSERT INTO output_log (time, job_id, output) VALUES (?, ?, ?)"
  (t, jobid, unlines output)

getAllOutputLogs :: DbPool -> IO OutputLogs
getAllOutputLogs pool =
  withConn pool $ \conn -> mkOutput <$> query_ conn "SELECT job_id, output FROM output_log ORDER BY time ASC"
 where
  mkOutput = foldr mergeOutput mempty
  mergeOutput (jobid, output) = HashMap.insertWith (++) jobid (lines output)
