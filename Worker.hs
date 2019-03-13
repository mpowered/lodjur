{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Main where

import           Control.Monad
import qualified Database.Redis               as Redis
import qualified Database.Redis.Queue         as Q
import           Lodjur.Messages
import           Options.Applicative          hiding (Success)
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
            handler conn jobid job

handler :: Redis.Connection -> Q.MsgId -> WorkerMsg -> IO ()
handler conn _jobid (CheckRequested repo sha suiteid) = do
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
  -- status conn jobid InProgress
  --
handler conn _jobid (RunCheck repo _sha _suiteid _name runid) = do
  putStrLn "Check Run"
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
