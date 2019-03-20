{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Main where

import           Control.Concurrent.Async
import           Control.Monad
import qualified Database.Redis               as Redis
import qualified Database.Redis.Queue         as Q
import qualified Lodjur.Database              as Db
import qualified Lodjur.Database.Checks       as Db
import           Lodjur.Messages
import           Options.Applicative          hiding (Success)
import           Prelude                      hiding (lookup)

import           GitHub
import           GitHub.Extra

import           Config
import qualified Build
import qualified Git

newtype Options = Options
  { configFile :: FilePath
  }

lodjur :: Parser Options
lodjur = Options
  <$> strOption
    (  long "config-file"
    <> metavar "PATH"
    <> short 'c'
    <> value "lodjur-worker.toml"
    <> help "Path to Lodjur Worker configuration file"
    )

main :: IO ()
main = start =<< execParser opts
 where
  opts = info
    (lodjur <**> helper)
    (fullDesc <> progDesc "Lodjur Worker" <> header
      "Performs check jobs submitted by Lodjur"
    )

start :: Options -> IO ()
start Options{..} = do
  cfg@Config{..} <- readConfiguration configFile

  conn <- Redis.checkedConnect redisConnectInfo

  concurrently_ (handleCheckRequests cfg conn) (handleCheckRuns cfg conn)

  where
    handleCheckRequests Config{..} conn =
      forever $ do
        x <- Redis.runRedis conn $ Q.poppush' checkRequestedQueue checkInProgressQueue 30
        case x of
          Nothing -> return ()  -- Timeout
          Just a ->
            case a of
              Left err ->
                putStrLn $ "Warning: unable to decode check request: " ++ err
              Right suite ->
                checkRequested conn gitEnv buildEnv suite

    handleCheckRuns Config{..} conn =
      forever $ do
        x <- Redis.runRedis conn $ Q.poppush' runRequestedQueue runInProgressQueue 30
        case x of
          Nothing -> return ()  -- Timeout
          Just a ->
            case a of
              Left err ->
                putStrLn $ "Warning: unable to decode check request: " ++ err
              Right run ->
                checkRun conn gitEnv buildEnv run

checkRequested :: Redis.Connection -> Git.Env -> Build.Env -> Id CheckSuite -> IO ()
checkRequested conn _ _ suiteid = do
  putStrLn "Check Suite"
  Redis.runRedis conn $
    Q.push workerQueue
      [ CreateCheckRun
        { checkSuiteId = suiteid
        , checkRunName = "nix build"
        }
      ]

checkRun :: Redis.Connection -> Git.Env -> Build.Env -> Id CheckRun -> IO ()
checkRun conn gitEnv buildEnv runid = do
  r <- Redis.runRedis conn $ Db.lookup (Db.checkRunKeyFromId runid)
  case r of
    Nothing ->
      putStrLn $ "missing check run details for " ++ show runid
    Just Db.CheckRun{..} -> do
      s <- Redis.runRedis conn $ Db.lookup (Db.checkSuiteKeyFromId checkSuiteId)
      case s of
        Nothing ->
          putStrLn $ "missing check suite details for " ++ show checkSuiteId
        Just Db.CheckSuite{..} -> do
          Redis.runRedis conn $
            Q.push workerQueue
              [ CheckRunInProgress
                { checkRunId = runid
                }
              ]

          let reporef = RepoRef (repoOwner repository) (repoName repository)
          workdir <- Git.checkout gitEnv reporef headSha
          Build.build buildEnv workdir "release.nix" "mpowered-services" [("railsEnv", "production")]

          Redis.runRedis conn $ do
            Q.push workerQueue
              [ CheckRunCompleted
                { checkRunId = runid
                , conclusion = Success
                }
              ]
            void $ Q.remove runInProgressQueue runid
