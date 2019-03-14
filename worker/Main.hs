{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Main where

import           Control.Monad
import qualified Database.Redis               as Redis
import qualified Database.Redis.Queue         as Q
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
  Config{..} <- readConfiguration configFile

  conn <- Redis.checkedConnect redisConnectInfo

  forever $ do
    x <- Redis.runRedis conn $ Q.pop workersQueue 30
    case x of
      Nothing -> return ()  -- Timeout
      Just jobid -> do
        a <- Redis.runRedis conn $ Q.lookupEither jobid
        case a of
          Nothing ->
            putStrLn $ "Warning: stale job " ++ show jobid
          Just (Left err) ->
            putStrLn $ "Warning: unable to decode job: " ++ err
          Just (Right job) -> do
            putStrLn $ show jobid ++ ": " ++ show job
            handler conn gitEnv buildEnv jobid job

handler :: Redis.Connection -> Git.Env -> Build.Env -> Q.MsgId -> WorkerMsg -> IO ()
handler conn _ _ _jobid (CheckRequested repo sha suiteid) = do
  putStrLn "Check Suite"
  Redis.runRedis conn $ do
    jobids <-
      Q.push lodjurQueue
        [ CreateCheckRun
          { repo = repo
          , headSha = sha
          , checkSuiteId = suiteid
          , checkRunName = "nix build"
          }
        ]
    Q.setTtl jobids (1*60*60)

handler conn gitEnv buildEnv _jobid (RunCheck repo sha _suiteid _name runid) = do
  putStrLn "Check Run"

  Redis.runRedis conn $ do
    jobids <-
      Q.push lodjurQueue
        [ CheckRunInProgress
          { repo = repo
          , checkRunId = runid
          }
        ]
    Q.setTtl jobids (1*60)

  check gitEnv buildEnv repo sha

  Redis.runRedis conn $ do
    jobids <-
      Q.push lodjurQueue
        [ CheckRunCompleted
          { repo = repo
          , checkRunId = runid
          , conclusion = Success
          }
        ]
    Q.setTtl jobids (1*60*60)

check :: Git.Env -> Build.Env -> RepoRef -> Sha -> IO ()
check gitEnv buildEnv repo sha = do
  workdir <- Git.checkout gitEnv repo sha
  Build.build buildEnv workdir "release.nix" "mpowered-services" [("railsEnv", "production")]

