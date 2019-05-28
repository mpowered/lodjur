{-# LANGUAGE RecordWildCards       #-}

module Lodjur.Core
  ( startCore
  , cancelCore
  , submit
  , subscribe
  , module Lodjur.Core.Types
  )
where

import           Control.Concurrent.Async
import           Control.Concurrent.STM
import           Control.Exception
import           Control.Monad
import           Control.Monad.IO.Class         ( liftIO )
import           Data.Aeson
import           Data.Bifunctor
import           Data.Int                       ( Int32 )
import           Data.Pool
import qualified Data.Text                     as Text
import           Data.Time.Clock                ( getCurrentTime )
import           Database.Beam.Postgres
import qualified Database.Beam.Postgres.Full   as Pg
import           Lodjur.Core.Types
import           Lodjur.Core.Util
import           Lodjur.Database               as DB
import           Lodjur.Database.Enum          as DB
import qualified Lodjur.GitHub                 as GH
import           Lodjur.Job                    as Job
import           Lodjur.RSpec                  as RSpec
import qualified Network.HTTP.Client           as HTTP

startCore :: GH.GitHubToken -> HTTP.Manager -> Pool Connection -> IO Core
startCore envGithubInstallationAccessToken envHttpManager envDbPool = do
  envJobQueue   <- newTQueueIO
  envReplyQueue <- newTQueueIO
  envEventChan  <- newBroadcastTChanIO
  let coreEnv = Env { .. }
  coreReplyHandler <- async (replyHandler coreEnv)
  return Core { .. }

cancelCore :: Core -> IO ()
cancelCore core = cancel (coreReplyHandler core)

replyHandler :: Env -> IO ()
replyHandler env = do
  e <- try $ forever $
    runCore env $ do
      (rep, assoc) <- liftIO $ atomically $ readTQueue (envReplyQueue env)
      handleReply rep assoc
  case first fromException e of
    Left (Just AsyncCancelled) -> return ()
    Left _ -> do
      putStrLn $ "Exception in replyHandler: " ++ show e
      replyHandler env
    Right () -> return ()

submit :: Core -> Maybe Int32 -> Job.Request -> IO ()
submit core parent job = runCore (coreEnv core) $ createJob parent job

subscribe :: Core -> IO (TChan Event)
subscribe core = atomically $ dupTChan $ envEventChan $ coreEnv core

notify :: Event -> CoreM ()
notify event = do
  Env {..} <- getEnv
  liftIO $ atomically $ writeTChan envEventChan event

upsertCommit :: GH.GitHubCommit -> Pg [Commit]
upsertCommit GH.GitHubCommit{..} =
  Pg.runPgInsertReturningList $
    Pg.insertReturning (dbCommits db)
      ( insertExpressions
        [ Commit
            { commitId             = default_
            , commitSha            = val_ ghcSha
            , commitOwner          = val_ ghcOwner
            , commitRepo           = val_ ghcRepo
            , commitBranch         = val_ ghcBranch
            , commitMessage        = val_ ghcMessage
            , commitAuthor         = val_ ghcAuthor
            , commitAuthorEmail    = val_ ghcAuthorEmail
            , commitCommitter      = val_ ghcCommitter
            , commitCommitterEmail = val_ ghcCommitterEmail
            , commitTimestamp      = val_ ghcTimestamp
            }
        ]
      )
      ( Pg.onConflict (Pg.conflictingFields (\c -> (commitOwner c, commitRepo c, commitSha c))) $
        Pg.onConflictUpdateInstead (\c -> (commitBranch c, commitMessage c, commitAuthor c, commitAuthorEmail c, commitCommitter c, commitCommitterEmail c, commitTimestamp c))
        -- Pg.onConflictUpdateSet (\o c ->
        --   coalesce_ [val_ ghcBranch] (current_ (commitBranch o))
        -- )
      )
      ( Just id )

createJob :: Maybe Int32 -> Job.Request -> CoreM ()
createJob parent req = do
  let Job.Request {..}     = req
      GH.GitHubCommit {..} = githubCommit
  Env {..} <- getEnv
  now      <- liftIO getCurrentTime
  [job]    <- database $ do
    [commit] <- upsertCommit githubCommit
    runInsertReturningList $ insert (dbJobs db) $ insertExpressions
      [ Job
          { jobId           = default_
          , jobName         = val_ name
          , jobCommit       = val_ (pk commit)
          , jobAction       = val_ (toJSON action)
          , jobStatus       = val_ (DbEnum Job.Queued)
          , jobConclusion   = val_ Nothing
          , jobCreatedAt    = val_ now
          , jobStartedAt    = val_ Nothing
          , jobCompletedAt  = val_ Nothing
          , jobParent       = val_ (JobKey parent)
          }
      ]
  let lodjurJobId = jobId job
  githubRun <-
    github $ GH.createCheckRunR (GH.N ghcOwner) (GH.N ghcRepo) $ (GH.newCheckRun (GH.N name) (GH.Sha ghcSha))
      { GH.newCheckRunStatus     = Just GH.Queued
      , GH.newCheckRunExternalId = Just (toExternalId lodjurJobId)
      }
  liftIO $ atomically $ writeTQueue envJobQueue (req, Associated { .. })
  notify (JobSubmitted lodjurJobId)
  where toExternalId = Text.pack . show

handleReply :: Reply -> Associated -> CoreM ()
handleReply rep Associated {..} = do
  let GH.GitHubCommit {..} = githubCommit
      GH.CheckRun {..}     = githubRun
  now <- liftIO getCurrentTime
  case rep of
    Started -> do
      database $ runUpdate $ update
        (dbJobs db)
        (\j -> mconcat
          [ jobStatus j <-. val_ (DbEnum Job.InProgress)
          , jobStartedAt j <-. val_ (Just now)
          ]
        )
        (\j -> jobId j ==. val_ lodjurJobId)
      github_
        $ GH.updateCheckRunR (GH.N ghcOwner) (GH.N ghcRepo) checkRunId
        $ GH.emptyUpdateCheckRun { GH.updateCheckRunStatus = Just GH.InProgress
                                 , GH.updateCheckRunStartedAt = Just now
                                 }
      notify (JobUpdated lodjurJobId)
    Requeued -> do
      database $ runUpdate $ update
        (dbJobs db)
        (\j -> mconcat
          [ jobStatus j <-. val_ (DbEnum Job.Queued)
          , jobStartedAt j <-. val_ Nothing
          ]
        )
        (\j -> jobId j ==. val_ lodjurJobId)
      github_
        $ GH.updateCheckRunR (GH.N ghcOwner) (GH.N ghcRepo) checkRunId
        $ GH.emptyUpdateCheckRun { GH.updateCheckRunStatus    = Just GH.Queued
                                 , GH.updateCheckRunStartedAt = Nothing
                                 }
      notify (JobUpdated lodjurJobId)
    LogOutput txt -> do
      database $ runInsert $ insert (dbLogs db) $ insertExpressions
        [ Log { logId        = default_
              , logJob       = val_ (JobKey lodjurJobId)
              , logCreatedAt = val_ now
              , logText      = val_ txt
              }
        ]
      notify $ LogsUpdated lodjurJobId
    Concluded Result {..} -> do
      database $ do
        runUpdate $ update
          (dbJobs db)
          (\j -> mconcat
            [ jobStatus j <-. val_ (DbEnum Job.Completed)
            , jobConclusion j <-. val_ (Just (DbEnum conclusion))
            , jobCompletedAt j <-. val_ (Just now)
            ]
          )
          (\j -> jobId j ==. val_ lodjurJobId)
        forM_ rspecResult $ \RSpecResult {..} -> do
          [rspec] <- runInsertReturningList $ insert (dbRspecs db) $ insertExpressions
            [ RSpec
                { rspecId           = default_
                , rspecJob          = val_ (JobKey lodjurJobId)
                , rspecDuration     = val_ (RSpec.rspecDuration rspecSummary)
                , rspecExampleCount = val_ (RSpec.rspecExampleCount rspecSummary)
                , rspecFailureCount = val_ (RSpec.rspecFailureCount rspecSummary)
                , rspecPendingCount = val_ (RSpec.rspecPendingCount rspecSummary)
                }
            ]
          runInsert $ insert (dbRspecTests db) $ insertExpressions $ map
            (\t ->
              let e = RSpec.testException t
              in
                RSpecTest
                  { rspectestId                 = default_
                  , rspectestRSpec              = val_ (RSpecKey (rspecId rspec))
                  , rspectestDescription        = val_ (RSpec.testDescription t)
                  , rspectestFullDescription    = val_ (RSpec.testFullDescription t)
                  , rspectestStatus             = val_ (RSpec.testStatus t)
                  , rspectestFilePath           = val_ (RSpec.testFilePath t)
                  , rspectestLineNumber         = val_ (RSpec.testLineNumber t)
                  , rspectestExceptionClass     = val_ (RSpec.exceptionClass <$> e)
                  , rspectestExceptionMessage   = val_ (RSpec.exceptionMessage <$> e)
                  , rspectestExceptionBacktrace = val_ (Text.unlines . RSpec.exceptionBacktrace <$> e)
                  }
            )
            rspecExamples
      github_
        $ GH.updateCheckRunR (GH.N ghcOwner) (GH.N ghcRepo) checkRunId
        $ GH.emptyUpdateCheckRun
            { GH.updateCheckRunStatus      = Just GH.Completed
            , GH.updateCheckRunConclusion  = Just $ case conclusion of
                                               Job.Success   -> GH.Success
                                               Job.Failure   -> GH.Failure
                                               Job.Neutral   -> GH.Neutral
                                               Job.Cancelled -> GH.Cancelled
            , GH.updateCheckRunCompletedAt = Just now
            , GH.updateCheckRunOutput      = output
            }
      mapM_ (createJob (Just lodjurJobId)) dependencies
      notify (JobUpdated lodjurJobId)
