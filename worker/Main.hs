{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TypeFamilies          #-}

module Main where

import           Control.Concurrent.Async
import           Control.Monad
import           Control.Monad.IO.Class
import           Data.Aeson                   hiding (Options, Success)
import           Data.ByteString              (ByteString)
import           Data.Text                    (Text)
import qualified Data.Text.Encoding           as Text
import qualified Data.UUID                    as UUID
import qualified Data.UUID.V4                 as UUID
import qualified Database.Redis               as Redis
import qualified Database.Redis.Queue         as Q
import           GHC.Generics
import qualified Lodjur.Database              as Db
import qualified Lodjur.Database.Checks       as Db
import qualified Lodjur.Database.Types        as Db
import           Lodjur.Messages
import           Options.Applicative          hiding (Success, Failure)
import           Prelude                      hiding (lookup)

import           GitHub
import           GitHub.Extra

import           Config
import           Env
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

setupEnv :: Config -> IO Env
setupEnv Config{..} = do
  redisConn <- Redis.checkedConnect redisConnectInfo
  gitEnv <- Git.setupEnv gitCfg
  return Env{..}

start :: Options -> IO ()
start Options{..} = do
  cfg <- readConfiguration configFile
  env <- setupEnv cfg

  concurrently_ (handleCheckRequests env) (handleCheckRuns env)

  where
    handleCheckRequests env@Env{..} =
      forever $ do
        x <- Redis.runRedis redisConn $ Q.poppush' checkRequestedQueue checkInProgressQueue 30
        case x of
          Nothing -> return ()  -- Timeout
          Just a ->
            case a of
              Left err ->
                putStrLn $ "Warning: unable to decode check request: " ++ err
              Right suite ->
                checkRequested env suite

    handleCheckRuns env@Env{..} =
      forever $ do
        x <- Redis.runRedis redisConn $ Q.poppush' runRequestedQueue runInProgressQueue 30
        case x of
          Nothing -> return ()  -- Timeout
          Just a ->
            case a of
              Left err ->
                putStrLn $ "Warning: unable to decode check request: " ++ err
              Right run ->
                checkRun env run

checkRequested :: Env -> Id CheckSuite -> IO ()
checkRequested Env{..} suiteid = do
  putStrLn "Check Suite"
  Redis.runRedis redisConn $
    Q.push workerQueue
      [ CreateCheckRun
        { checkSuiteId = suiteid
        , checkRunName = "nix build"
        , checkRunExternalId = Nothing
        , checkRunOutput = Nothing
        }
      ]

lookupRun :: Env -> Id CheckRun -> IO (Maybe (Db.CheckSuite, Db.CheckRun, Maybe Run))
lookupRun Env{..} runid = do
  r <- Redis.runRedis redisConn $ Db.lookup (Db.checkRunKeyFromId runid)
  case r of
    Nothing -> do
      putStrLn $ "missing check run details for run " ++ show runid
      return Nothing
    Just run -> do
      s <- Redis.runRedis redisConn $ Db.lookup (Db.checkSuiteKeyFromId (Db.checkSuiteId run))
      case s of
        Nothing -> do
          putStrLn $ "missing check suite details for suite " ++ show (Db.checkSuiteId run)
          return Nothing
        Just suite -> do
          rundata <- getRunData (Db.externalId run)
          return $ Just (suite, run, rundata)
  where
    getRunData Nothing = return Nothing
    getRunData (Just externalid) = do
      d <- Redis.runRedis redisConn $ Db.lookup (runKeyFromText externalid)
      case d of
        Nothing -> do
          putStrLn $ "missing check run data " ++ show externalid
          return Nothing
        Just dat -> return $ Just dat

data Run
  = Build
  | RSpec { rpecApp :: Text }
  deriving (Show, Eq, Ord, Generic)

instance ToJSON Run where
  toJSON = genericToJSON jsonOptions

instance FromJSON Run where
  parseJSON = genericParseJSON jsonOptions

instance Db.HasKey Run where
  data Key Run = RunKey ByteString
  redisKey (RunKey bs) = "run:" <> bs

randomRunKey :: IO (Db.Key Run)
randomRunKey = do
  bs <- UUID.toASCIIBytes  <$> UUID.nextRandom
  return $ RunKey bs

runKeyFromText :: Text -> Db.Key Run
runKeyFromText = RunKey . Text.encodeUtf8

runKeyToText :: Db.Key Run -> Text
runKeyToText (RunKey bs) = Text.decodeUtf8 bs

createRun :: Id CheckSuite -> Name CheckRun -> Run -> Redis.Redis ()
createRun suiteid name rundata = do
  runKey <- liftIO randomRunKey
  Db.put_ runKey rundata
  Q.push workerQueue
    [ CreateCheckRun
      { checkSuiteId = suiteid
      , checkRunName = name
      , checkRunExternalId = Just (runKeyToText runKey)
      , checkRunOutput = Nothing
      }
    ]

withRun :: Env -> Id CheckRun -> (Db.CheckSuite -> Db.CheckRun -> Maybe Run -> IO ()) -> IO ()
withRun env runid f = do
  r <- lookupRun env runid
  maybe (return ()) (\(cs, cr, d) -> f cs cr d) r

checkRun :: Env -> Id CheckRun -> IO ()
checkRun env@Env{..} runid =
  withRun env runid $ \Db.CheckSuite{..} Db.CheckRun{checkSuiteId} rundata -> do
    Redis.runRedis redisConn $
      Q.push workerQueue
        [ CheckRunInProgress
          { checkRunId = runid
          , checkRunOutput = Nothing
          }
        ]

    case rundata of
      Nothing -> do
        putStrLn "Nix Build"
        let reporef = RepoRef (repoOwner repository) (repoName repository)
        workdir <- Git.checkout gitEnv reporef headSha
        Build.build buildCfg workdir "release.nix" "mpowered-services" [("railsEnv", "production")]

        Redis.runRedis redisConn $ do
          Q.push workerQueue
            [ CheckRunCompleted
              { checkRunId = runid
              , conclusion = Success
              , checkRunOutput = Just $ CheckRunOutput
                { checkRunOutputTitle = "Nix Build"
                , checkRunOutputSummary = "Build completed successfully."
                , checkRunOutputText = Nothing
                }
              }
            ]
          void $ Q.remove runInProgressQueue runid

          createRun checkSuiteId "rspec toolkit" (RSpec "toolkit")
          createRun checkSuiteId "rspec sms"     (RSpec "sms")
          createRun checkSuiteId "rspec beagle"  (RSpec "beagle")
          -- Q.push workerQueue
          --   [ CreateCheckRun
          --     { checkSuiteId = checkSuiteId
          --     , checkRunName = "rspec toolkit"
          --     , checkRunExternalId = Just "toolkit"
          --     , checkRunOutput = Nothing
          --     }
          --   , CreateCheckRun
          --     { checkSuiteId = checkSuiteId
          --     , checkRunName = "rspec sms"
          --     , checkRunExternalId = Just "sms"
          --     , checkRunOutput = Nothing
          --     }
          --   , CreateCheckRun
          --     { checkSuiteId = checkSuiteId
          --     , checkRunName = "rspec beagle"
          --     , checkRunExternalId = Just "beagle"
          --     , checkRunOutput = Nothing
          --     }
          --   ]

      Just (RSpec app) -> do
        putStrLn $ "RSpec " ++ show app

        -- TODO RSPEC

        Redis.runRedis redisConn $ do
          Q.push workerQueue
            [ CheckRunCompleted
              { checkRunId = runid
              , conclusion = Success
              , checkRunOutput = Just $ CheckRunOutput
                { checkRunOutputTitle = "RSpec"
                , checkRunOutputSummary = "Tests completed successfully."
                , checkRunOutputText = Nothing
                }
              }
            ]
          void $ Q.remove runInProgressQueue runid

      _ ->
        Redis.runRedis redisConn $ do
          Q.push workerQueue
            [ CheckRunCompleted
              { checkRunId = runid
              , conclusion = Failure
              , checkRunOutput = Just $ CheckRunOutput
                { checkRunOutputTitle = "Unknown Run Request"
                , checkRunOutputSummary = "Run failed as the worker didn't recognise the request."
                , checkRunOutputText = Nothing
                }
              }
            ]
          void $ Q.remove runInProgressQueue runid
