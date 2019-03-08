{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Main where

import           Control.Concurrent
import           Control.Monad
import qualified Database.Redis               as Redis
import           Lodjur.Jobs
import           Lodjur.JobQueue
import           Network.HTTP.Client
import           Network.HTTP.Client.TLS
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
start Options {..} = do
  Config {..} <- readConfiguration configFile

  mgr <- newManager tlsManagerSettings
  conn <- Redis.checkedConnect redisConnectInfo
  githubInstallationAccessToken <- newMVar Nothing

  forever $ do
    x <- poppush conn "requested" "checking" 10
    case x of
      Nothing -> return ()  -- Timeout
      Just jobid -> do
        a <- lookup conn jobid
        case a of
          Nothing ->
            putStrLn $ "Warning: stale job " ++ show jobid
          Just (Left err) -> do
            putStrLn $ "Warning: unable to decode job: " ++ err
            move conn "checking" "failed" jobid
          Just (Right job) -> do
            putStrLn $ show jobid ++ ": " ++ show job
            handler conn jobid job

handler :: Redis.Connection -> JobId -> Job -> IO ()
handler conn jobid (CheckJob repo sha suiteid) = do
  setTtl conn jobid (4*60*60)
  status conn jobid InProgress
