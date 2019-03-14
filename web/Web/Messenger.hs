{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Web.Messenger where

import           Control.Monad
import           Control.Monad.Trans.Maybe
import           Data.Time.Clock
import           Lodjur.Messages
import           Lodjur.GitHub

import qualified Database.Redis                as Redis
import qualified Database.Redis.Queue          as Q

import           GitHub
import           GitHub.Data.Id
import           GitHub.Data.Name
import           GitHub.Extra
import           GitHub.Endpoints.Checks

nextMsg :: Redis.Connection -> Q.Queue -> IO LodjurMsg
nextMsg conn name = do
  n <-  Redis.runRedis conn $ runMaybeT $ do
          msgId <- MaybeT $ Q.pop name 60
          MaybeT $ Q.lookup msgId
  case n of
    Just msg -> return msg
    Nothing -> nextMsg conn name

messenger :: Redis.Connection -> GitHubToken -> IO ()
messenger redis auth = forever $ do
  msg <- nextMsg redis lodjurQueue
  tok <- getToken auth
  case tok of
    Just t -> handleMsg redis t msg
    Nothing -> error "failed to refresh github token"

handleMsg :: Redis.Connection -> Token -> LodjurMsg -> IO ()
handleMsg redis tok (CreateCheckRun repo sha suiteid runname) = do
  r <- createCheckRun (OAuth tok) (N $ owner repo) (N $ name repo) $
    NewCheckRun
      { newCheckRunName         = N runname
      , newCheckRunHeadSha      = sha
      , newCheckRunDetailsUrl   = Nothing
      , newCheckRunExternalId   = Nothing
      , newCheckRunStatus       = Nothing
      , newCheckRunStartedAt    = Nothing
      , newCheckRunConclusion   = Nothing
      , newCheckRunCompletedAt  = Nothing
      , newCheckRunOutput       = Nothing
      , newCheckRunActions      = Nothing
      }
  case r of
    Left _ -> return ()
    Right CheckRun{..} ->
      Redis.runRedis redis $ do
        jobids <-
          Q.push workersQueue
            [ RunCheck
              { checkRunId = untagId checkRunId
              , checkRunName = runname
              , repo = repo
              , headSha = sha
              , checkSuiteId = suiteid
              }
            ]
        Q.setTtl jobids (1*60*60)

handleMsg _redis tok (CheckRunCompleted repo runid conclusion) = do
  now <- getCurrentTime
  let c = case conclusion of
            Cancelled -> "cancelled"
            TimedOut -> "timed_out"
            Failed -> "failed"
            Neutral -> "neutral"
            Success -> "success"
  r <- updateCheckRun (OAuth tok) (N $ owner repo) (N $ name repo) (Id runid) $
    UpdateCheckRun
      { updateCheckRunName         = Nothing
      , updateCheckRunDetailsUrl   = Nothing
      , updateCheckRunExternalId   = Nothing
      , updateCheckRunStatus       = Just "completed"
      , updateCheckRunStartedAt    = Nothing
      , updateCheckRunConclusion   = Just c
      , updateCheckRunCompletedAt  = Just now
      , updateCheckRunOutput       = Nothing
      , updateCheckRunActions      = Nothing
      }
  case r of
    Left _ -> return ()
    Right a -> putStrLn $ "updated run: " ++ show a
