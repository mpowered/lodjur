{-# LANGUAGE RecordWildCards       #-}

module Lodjur.Core
  ( startCore
  , cancelCore
  , submit
  , subscribe
  , module Lodjur.Core.Types
  )where

import           Control.Concurrent.Async
import           Control.Concurrent.STM
import           Control.Exception
import           Control.Monad
import           Control.Monad.IO.Class         ( liftIO )
import           Data.Aeson
import           Data.Int                       ( Int32 )
import           Data.Pool
import qualified Data.Text                     as Text
import           Data.Time.Clock                ( getCurrentTime )
import           Lodjur.Core.Types
import           Lodjur.Core.Util
import qualified Lodjur.Core.Websocket         as WS
import           Lodjur.Database               as DB
import           Lodjur.Database.Enum          as DB
import           Lodjur.GitHub                  ( untagName
                                                , untagSha
                                                )
import qualified Lodjur.GitHub                 as GH
import           Lodjur.Job                    as Job
import qualified Network.HTTP.Client           as HTTP

startCore :: GH.GitHubToken -> HTTP.Manager -> Pool Connection -> IO Core
startCore envGithubInstallationAccessToken envHttpManager envDbPool = do
  envJobQueue   <- newTQueueIO
  envReplyQueue <- newTQueueIO
  envEventChan  <- newBroadcastTChanIO
  let coreEnv = Env { .. }
  coreReplyHandler <- async (replyHandler coreEnv)
  let coreWebSocketApp = WS.serverApp coreEnv
  return Core { .. }

cancelCore :: Core -> IO ()
cancelCore core = cancel (coreReplyHandler core)

replyHandler :: Env -> IO ()
replyHandler env = forever $
  catch
    ( runCore env $ do
        (rep, assoc) <- liftIO $ atomically $ readTQueue (envReplyQueue env)
        handleReply rep assoc
    )
    ( \(SomeException e) -> putStrLn $ "Exception in replyHandler: " ++ show e )

submit :: Core -> Maybe Int32 -> Job.Request -> IO ()
submit core parent job = runCore (coreEnv core) $ createJob parent job

subscribe :: Core -> IO (TChan Event)
subscribe core = atomically $ dupTChan $ envEventChan $ coreEnv core

notify :: Event -> CoreM ()
notify event = do
  Env {..} <- getEnv
  liftIO $ atomically $
    writeTChan envEventChan event

createJob :: Maybe Int32 -> Job.Request -> CoreM ()
createJob parent req = do
  let Job.Request {..}    = req
      GH.Source {..}      = githubSource
      GH.SimpleOwner {..} = owner
  Env {..} <- getEnv
  now      <- liftIO getCurrentTime
  [job]    <- database $ runInsertReturningList (jobs db) $ insertExpressions
    [ JobT { jobId           = default_
           , jobName         = val_ (untagName name)
           , jobSrcSha       = val_ (untagSha sha)
           , jobSrcBranch    = val_ branch
           , jobSrcOwner     = val_ (untagName simpleOwnerLogin)
           , jobSrcRepo      = val_ (untagName repo)
           , jobSrcMessage   = val_ (GH.eventCheckSuiteCommitMessage <$> commit)
           , jobSrcCommitter = val_ (GH.eventCheckSuiteUserName <$> (GH.eventCheckSuiteCommitCommitter =<< commit))
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
    github $ GH.createCheckRunR simpleOwnerLogin repo $ (GH.newCheckRun name sha)
      { GH.newCheckRunStatus     = Just GH.Queued
      , GH.newCheckRunExternalId = Just (toExternalId lodjurJobId)
      }
  liftIO $ atomically $ writeTQueue envJobQueue (req, Associated { .. })
  notify JobSubmitted
 where
  toExternalId = Text.pack . show

handleReply :: Reply -> Associated -> CoreM ()
handleReply rep Associated {..} = do
  let GH.Source {..}      = githubSource
      GH.CheckRun {..}    = githubRun
      GH.SimpleOwner {..} = owner
  now <- liftIO getCurrentTime
  case rep of
    Started -> do
      database $ runUpdate $ update
        (jobs db)
        (\j ->
          [ jobStatus j <-. val_ (DbEnum Job.InProgress)
          , jobStartedAt j <-. val_ (Just now)
          ]
        )
        (\j -> jobId j ==. val_ lodjurJobId)
      github_
        $ GH.updateCheckRunR simpleOwnerLogin repo checkRunId
        $ GH.emptyUpdateCheckRun
            { GH.updateCheckRunStatus    = Just GH.InProgress
            , GH.updateCheckRunStartedAt = Just now
            }
    Concluded Result{..} -> do
      database $ runUpdate $ update
        (jobs db)
        (\j ->
          [ jobStatus j <-. val_ (DbEnum Job.Completed)
          , jobConclusion j <-. val_ (Just (DbEnum conclusion))
          , jobCompletedAt j <-. val_ (Just now)
          ]
        )
        (\j -> jobId j ==. val_ lodjurJobId)
      github_
        $ GH.updateCheckRunR simpleOwnerLogin repo checkRunId
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
  notify JobUpdated
