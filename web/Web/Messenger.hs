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
  print msg
  tok <- getToken auth
  case tok of
    Just t -> handleMsg redis t msg
    Nothing -> error "failed to refresh github token"

owner :: RepoRef -> Name Owner
owner = simpleOwnerLogin . repoRefOwner

handleMsg :: Redis.Connection -> Token -> LodjurMsg -> IO ()
handleMsg redis tok (CreateCheckRun repo sha suiteid runname) = do
  r <- createCheckRun (OAuth tok) (owner repo) (repoRefRepo repo) $
    (newCheckRun runname sha)
      { newCheckRunStatus       = Just Queued
      }
  case r of
    Left _ -> return ()
    Right CheckRun{..} ->
      Redis.runRedis redis $ do
        jobids <-
          Q.push workersQueue
            [ RunCheck
              { checkRunId = checkRunId
              , checkRunName = runname
              , repo = repo
              , headSha = sha
              , checkSuiteId = suiteid
              }
            ]
        Q.setTtl jobids (1*60*60)

handleMsg _redis tok (CheckRunInProgress repo runid) = do
  r <- updateCheckRun (OAuth tok) (owner repo) (repoRefRepo repo) runid $
    emptyUpdateCheckRun
      { updateCheckRunStatus       = Just InProgress
      }
  case r of
    Left _ -> return ()
    Right a -> putStrLn $ "updated run: " ++ show a

handleMsg _redis tok (CheckRunCompleted repo runid conclusion) = do
  now <- getCurrentTime
  r <- updateCheckRun (OAuth tok) (owner repo) (repoRefRepo repo) runid $
    emptyUpdateCheckRun
      { updateCheckRunStatus       = Just Completed
      , updateCheckRunConclusion   = Just conclusion
      , updateCheckRunCompletedAt  = Just now
      }
  case r of
    Left _ -> return ()
    Right a -> putStrLn $ "updated run: " ++ show a
