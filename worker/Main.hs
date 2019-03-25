{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TypeFamilies          #-}

module Main where

import           Control.Concurrent.Async
import           Control.Monad
import           Control.Error
import           Control.Exception
import           Control.Monad.IO.Class
import           Data.Aeson                   hiding (Options, Success)
import           Data.ByteString              (ByteString)
import           Data.Text                    (Text)
import qualified Data.Text                    as Text
import qualified Data.Text.Encoding           as Text
import qualified Data.UUID                    as UUID
import qualified Data.UUID.V4                 as UUID
import qualified Data.Vector                  as Vec
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
import qualified RSpec

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
              Left errmsg ->
                putStrLn $ "Warning: unable to decode check request: " ++ errmsg
              Right suite ->
                checkRequested env suite

    handleCheckRuns env@Env{..} =
      forever $ do
        x <- Redis.runRedis redisConn $ Q.poppush' runRequestedQueue runInProgressQueue 30
        case x of
          Nothing -> return ()  -- Timeout
          Just a ->
            case a of
              Left errmsg ->
                putStrLn $ "Warning: unable to decode check request: " ++ errmsg
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
  r <- runExceptT $ do
    run     <- Redis.runRedis redisConn (Db.lookup (Db.checkRunKeyFromId runid))
              !? ("missing check run details for run " ++ show runid)
    suite   <- Redis.runRedis redisConn (Db.lookup (Db.checkSuiteKeyFromId (Db.checkSuiteId run)))
                !? ("missing check suite details for suite " ++ show (Db.checkSuiteId run))
    return (run, suite)
  case r of
    Left errmsg -> do
      putStrLn errmsg
      return Nothing
    Right (run, suite) -> do
      rundata <- case Db.externalId run of
                  Just dataid -> Redis.runRedis redisConn (Db.lookup (runKeyFromText dataid))
                  Nothing -> return Nothing
      return $ Just (suite, run, rundata)

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
        br <- try $ Build.build buildCfg workdir ".lodjur/build.nix"
        case br of
          Left (Build.BuildError code stdout _stderr) -> do
            let o = Vec.fromList (lines stdout)
                opos = max 0 (Vec.length o - 10)
                otail = Vec.drop opos o
                otxt = Text.pack $ unlines $ Vec.toList otail
            Redis.runRedis redisConn $ do
              void $ Q.remove runInProgressQueue runid
              Q.push workerQueue
                [ CheckRunCompleted
                  { checkRunId = runid
                  , conclusion = Failure
                  , checkRunOutput = Just $ CheckRunOutput
                    { checkRunOutputTitle = "Build failed"
                    , checkRunOutputSummary = "Build returned an error: exit code " <> Text.pack (show code)
                    , checkRunOutputText = Just otxt
                    }
                  }
                ]
          Right () ->
            Redis.runRedis redisConn $ do
              void $ Q.remove runInProgressQueue runid
              createRun checkSuiteId "rspec toolkit" (RSpec "toolkit")
              createRun checkSuiteId "rspec sms"     (RSpec "sms")
              createRun checkSuiteId "rspec beagle"  (RSpec "beagle")
              Q.push workerQueue
                [ CheckRunCompleted
                  { checkRunId = runid
                  , conclusion = Success
                  , checkRunOutput = Nothing
                  }
                ]

      Just (RSpec app) -> do
        putStrLn $ "RSpec " ++ show app
        let reporef = RepoRef (repoOwner repository) (repoName repository)
        workdir <- Git.checkout gitEnv reporef headSha
        br <- try $ Build.build buildCfg workdir ".lodjur/build.nix"
        case br of
          Left (Build.BuildError code stdout _stderr) -> do
            let o = Vec.fromList (lines stdout)
                opos = max 0 (Vec.length o - 10)
                otail = Vec.drop opos o
                otxt = Text.pack $ unlines $ Vec.toList otail
            Redis.runRedis redisConn $ do
              void $ Q.remove runInProgressQueue runid
              Q.push workerQueue
                [ CheckRunCompleted
                  { checkRunId = runid
                  , conclusion = Failure
                  , checkRunOutput = Just $ CheckRunOutput
                    { checkRunOutputTitle = "Build failed"
                    , checkRunOutputSummary = "Build returned an error: exit code " <> Text.pack (show code)
                    , checkRunOutputText = Just otxt
                    }
                  }
                ]
          Right () -> do
            rr <- try $ RSpec.rspec workdir [Text.unpack app]

            case rr of
              Left (RSpec.RSpecError code) ->
                Redis.runRedis redisConn $ do
                  void $ Q.remove runInProgressQueue runid
                  Q.push workerQueue
                    [ CheckRunCompleted
                      { checkRunId = runid
                      , conclusion = Failure
                      , checkRunOutput = Just $ CheckRunOutput
                        { checkRunOutputTitle = "RSpec failed"
                        , checkRunOutputSummary = "RSpec returned an error: exit code " <> Text.pack (show code)
                        , checkRunOutputText = Nothing
                        }
                      }
                    ]
              Right () ->
                Redis.runRedis redisConn $ do
                  void $ Q.remove runInProgressQueue runid
                  Q.push workerQueue
                    [ CheckRunCompleted
                      { checkRunId = runid
                      , conclusion = Success
                      , checkRunOutput = Nothing
                      }
                    ]

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
