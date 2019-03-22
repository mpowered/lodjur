{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}

module Messenger where

import           Control.Concurrent
import           Control.Monad
import           Data.Aeson
import           Data.Time.Clock
import           Lodjur.Messages
import           Lodjur.GitHub

import qualified Lodjur.Database               as Db
import qualified Lodjur.Database.Checks        as Db

import qualified Database.Redis                as Redis
import qualified Database.Redis.Queue          as Q

import           GitHub
import           GitHub.Extra
import           GitHub.Endpoints.Checks

messenger :: Redis.Connection -> GitHubToken -> IO ()
messenger redis auth = forever $ do
  msg <- nextMsg redis workerQueue
  print msg
  tok <- ensureToken auth
  handleMsg redis tok msg
  where
    ensureToken a = do
      tok <- getToken auth
      case tok of
        Just t -> return t
        Nothing -> do
          putStrLn "failed to refresh github token"
          threadDelay (10 * 1000000)
          ensureToken a

nextMsg :: FromJSON a => Redis.Connection -> Q.Queue a -> IO a
nextMsg conn name = do
  r <- Redis.runRedis conn $ Q.pop [name] 60
  case r of
    Just (_, msg) -> return msg
    Nothing -> nextMsg conn name

owner :: RepoRef -> Name Owner
owner = simpleOwnerLogin . repoRefOwner

handleMsg :: Redis.Connection -> Token -> WorkerMsg -> IO ()
handleMsg conn tok (CreateCheckRun suiteid runname externalid output) = do
  s <- Redis.runRedis conn $ Db.lookup (Db.checkSuiteKeyFromId suiteid)
  case s of
    Nothing ->
      putStrLn $ "create run for missing suite: " ++ show suiteid
    Just Db.CheckSuite{..} -> do
      r <- createCheckRun (OAuth tok) (simpleOwnerLogin $ repoOwner repository) (repoName repository) $
        (newCheckRun runname headSha)
          { newCheckRunStatus          = Just Queued
          , newCheckRunExternalId      = externalid
          , newCheckRunOutput          = output
          }
      putStrLn $ "create run: " ++ show r
      case r of
        Left err -> putStrLn $ "create failed: " ++ show err
        Right run -> putStrLn $ "created: " ++ show run

handleMsg conn tok (CheckRunInProgress runid output') = do
  run <- Redis.runRedis conn $ Db.lookup (Db.checkRunKeyFromId runid)
  case run of
    Nothing ->
      putStrLn $ "update run for missing run: " ++ show runid
    Just Db.CheckRun{..} -> do
      suite <- Redis.runRedis conn $ Db.lookup (Db.checkSuiteKeyFromId checkSuiteId)
      case suite of
        Nothing ->
          putStrLn $ "suite missing for update run: " ++ show checkSuiteId
        Just Db.CheckSuite{..} -> do
          r <- updateCheckRun (OAuth tok) (simpleOwnerLogin $ repoOwner repository) (repoName repository) runid $
            emptyUpdateCheckRun
              { updateCheckRunStatus       = Just InProgress
              , updateCheckRunOutput       = output'
              }
          putStrLn $ "update run: " ++ show r
          case r of
            Left err -> putStrLn $ "update failed: " ++ show err
            Right run -> putStrLn $ "updated: " ++ show run

handleMsg conn tok (CheckRunCompleted runid concl output') = do
  now <- getCurrentTime
  run <- Redis.runRedis conn $ Db.lookup (Db.checkRunKeyFromId runid)
  case run of
    Nothing ->
      putStrLn $ "update run for missing run: " ++ show runid
    Just Db.CheckRun{..} -> do
      suite <- Redis.runRedis conn $ Db.lookup (Db.checkSuiteKeyFromId checkSuiteId)
      case suite of
        Nothing ->
          putStrLn $ "suite missing for update run: " ++ show checkSuiteId
        Just Db.CheckSuite{..} -> do
          r <- updateCheckRun (OAuth tok) (simpleOwnerLogin $ repoOwner repository) (repoName repository) runid $
            emptyUpdateCheckRun
              { updateCheckRunStatus       = Just Completed
              , updateCheckRunConclusion   = Just concl
              , updateCheckRunCompletedAt  = Just now
              , updateCheckRunOutput       = output'
              }
          putStrLn $ "complete run: " ++ show r
          case r of
            Left err -> putStrLn $ "update failed: " ++ show err
            Right run -> putStrLn $ "completed: " ++ show run
