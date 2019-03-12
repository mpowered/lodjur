{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Main where

import           Control.Monad
import qualified Database.Redis               as Redis
import qualified Database.Redis.Queue         as Q
import qualified Lodjur.Jobs                  as Jobs
import           Options.Applicative
import           Worker.Config
import           Prelude                      hiding (lookup)

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
    x <- Redis.runRedis conn $ Q.pop "worker" 30
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
            handler conn jobid job

handler :: Redis.Connection -> Q.MsgId -> Jobs.Job -> IO ()
handler conn _jobid (Jobs.CheckRequested repo sha suiteid) = do
  putStrLn "Check Suite"
  Redis.runRedis conn $ do
    jobids <-
      Q.push "messenger"
        [ Jobs.CreateCheckRun
          { jobRepo = repo
          , jobHeadSha = sha
          , jobSuiteId = suiteid
          , jobName = "nix build"
          }
        ]
    Q.setTtl jobids (1*60*60)
  -- status conn jobid InProgress
  --
handler conn _jobid (Jobs.CheckRun repo _sha _suiteid _name runid) = do
  putStrLn "Check Run"
  Redis.runRedis conn $ do
    jobids <-
      Q.push "messenger"
        [ Jobs.CompleteCheckRun
          { jobRepo = repo
          , jobRunId = runid
          , jobConclusion = Jobs.Success
          }
        ]
    Q.setTtl jobids (1*60*60)
