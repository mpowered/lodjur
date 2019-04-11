{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ExtendedDefaultRules  #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TypeFamilies          #-}

module Main where

import           Control.Concurrent.Async
import           Control.Monad
import           Control.Error
import           Control.Monad.Catch          (MonadMask, bracket)
import           Control.Monad.IO.Class
import           Control.Monad.Log
import           Control.Monad.Reader
import           Data.Aeson                   hiding (Options, Success)
import           Data.Aeson.Encode.Pretty     (encodePretty)
import           Data.ByteString              (ByteString)
import qualified Data.ByteString.Lazy         as ByteString
import           Data.Text                    (Text)
import qualified Data.Text                    as Text
import qualified Data.Text.Encoding           as Text
import           Data.Text.Prettyprint.Doc
import qualified Data.UUID                    as UUID
import qualified Data.UUID.V4                 as UUID
import qualified Data.Vector                  as Vec
import           GHC.Generics
import           Lodjur.Logging
import           Lodjur.Manager
import qualified Lodjur.Manager.Messages      as Msg
import           Options.Applicative          hiding (Success, Failure)
import           System.Directory
import           Prelude                      hiding (lookup)

import           Network.Socket               (withSocketsDo)
import           Network.WebSockets

import           GitHub
import           GitHub.Extra

import           Config
import           Env
import qualified Build
import qualified Git
import qualified RSpec
import qualified RSpec.Results                as RSpec
import           Types

default (Text)

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
  gitEnv <- Git.setupEnv gitCfg
  let logTarget = maybe LogStdout LogFile logFile

  withSocketsDo $
    runManagerClient managerCI $ \messageConn ->
      runWorker Env{..} (handshake app)

handshake :: Worker () -> Worker ()
handshake a = do
  Env{..} <- ask
  msg <- receiveMsg messageConn
  case msg of
    Just Msg.Greet -> do
      sendMsg messageConn (Msg.Register "client")
      a
    Nothing -> return ()

app :: Worker ()
app = do
  Env{..} <- ask
  logInfo "Worker ready"
  forever $ do
    msg <- receiveMsg messageConn
    case msg of
      Msg.Build _name src -> build src >>= sendMsg messageConn
      unsupported -> do
        logError $ "Unsupported message:" <+> nest 4 (viaShow unsupported)
        sendMsg messageConn $ Msg.Completed Failure Nothing

withWorkDir
  :: (MonadIO io, MonadMask io)
  => Env
  -> RepoRef
  -> Sha
  -> (FilePath -> io a)
  -> io a
withWorkDir Env {..} repo sha
  = bracket
    (liftIO $ Git.checkout gitEnv repo sha)
    (liftIO . removeDirectoryRecursive)

build :: Msg.Source -> Worker Msg.Reply
build Msg.Source{..} = do
  env@Env{..} <- ask
  logInfo "Build started"
  reply <- withWorkDir env (RepoRef owner repo) sha $ \workdir ->
    Build.build workdir ".lodjur/build.nix"
  logInfo $ "Build completed:" <+> viaShow reply
  return reply

      -- Return error
      {-
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
              , checkRunOutputAnnotations = []
              }
            }
          ]
    Right () ->
      Redis.runRedis redisConn $ do
        void $ Q.remove runInProgressQueue runid
        createRun checkSuiteId "rspec toolkit-1" (RSpec "toolkit1")
        createRun checkSuiteId "rspec toolkit-2" (RSpec "toolkit2")
        createRun checkSuiteId "rspec toolkit-3" (RSpec "toolkit3")
        createRun checkSuiteId "rspec sms"     (RSpec "sms")
        createRun checkSuiteId "rspec beagle"  (RSpec "beagle")
        Q.push workerQueue
          [ CheckRunCompleted
            { checkRunId = runid
            , conclusion = Success
            , checkRunOutput = Nothing
            }
          ]
      -}

{-
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
                    , checkRunOutputAnnotations = []
                    }
                  }
                ]
          Right () ->
            Redis.runRedis redisConn $ do
              void $ Q.remove runInProgressQueue runid
              createRun checkSuiteId "rspec toolkit-1" (RSpec "toolkit1")
              createRun checkSuiteId "rspec toolkit-2" (RSpec "toolkit2")
              createRun checkSuiteId "rspec toolkit-3" (RSpec "toolkit3")
              createRun checkSuiteId "rspec sms"     (RSpec "sms")
              createRun checkSuiteId "rspec beagle"  (RSpec "beagle")
              Q.push workerQueue
                [ CheckRunCompleted
                  { checkRunId = runid
                  , conclusion = Success
                  , checkRunOutput = Nothing
                  }
                ]
        removeDirectoryRecursive workdir

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
            removeDirectoryRecursive workdir
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
                    , checkRunOutputAnnotations = []
                    }
                  }
                ]
          Right () -> do
            rr <- try $ RSpec.rspec workdir (Text.unpack app)

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
                        , checkRunOutputAnnotations = []
                        }
                      }
                    ]
              Left RSpec.RSpecParseFailed ->
                Redis.runRedis redisConn $ do
                  void $ Q.remove runInProgressQueue runid
                  Q.push workerQueue
                    [ CheckRunCompleted
                      { checkRunId = runid
                      , conclusion = Failure
                      , checkRunOutput = Just $ CheckRunOutput
                        { checkRunOutputTitle = "RSpec failed"
                        , checkRunOutputSummary = "Unable to parse RSpec output"
                        , checkRunOutputText = Nothing
                        , checkRunOutputAnnotations = []
                        }
                      }
                    ]
              Right RSpec.RSpecResult{..} -> do
                let RSpec.RSpecSummary{..} = rspecSummary
                    failing = filter (\t -> RSpec.testStatus t == "failed") rspecExamples
                    ann = map mkAnnotation failing
                if rspecFailureCount == 0
                  then
                    Redis.runRedis redisConn $ do
                      void $ Q.remove runInProgressQueue runid
                      Q.push workerQueue
                        [ CheckRunCompleted
                          { checkRunId = runid
                          , conclusion = Success
                          , checkRunOutput = Nothing
                          }
                        ]
                  else
                    Redis.runRedis redisConn $ do
                      void $ Q.remove runInProgressQueue runid
                      Q.push workerQueue
                        [ CheckRunCompleted
                          { checkRunId = runid
                          , conclusion = Failure
                          , checkRunOutput = Just $ CheckRunOutput
                            { checkRunOutputTitle = "RSpec failed"
                            , checkRunOutputSummary = Text.pack (show rspecFailureCount) <> " tests failed"
                            , checkRunOutputText = Nothing
                            , checkRunOutputAnnotations = take 50 ann
                            }
                          }
                        ]
        removeDirectoryRecursive workdir

      _ ->
        Redis.runRedis redisConn $ do
          void $ Q.remove runInProgressQueue runid
          Q.push workerQueue
            [ CheckRunCompleted
              { checkRunId = runid
              , conclusion = Failure
              , checkRunOutput = Just $ CheckRunOutput
                { checkRunOutputTitle = "Unknown Run Request"
                , checkRunOutputSummary = "Run failed as the worker didn't recognise the request."
                , checkRunOutputText = Nothing
                , checkRunOutputAnnotations = []
                }
              }
            ]

mkAnnotation :: RSpec.TestResult -> CheckRunAnnotation
mkAnnotation RSpec.TestResult{..} =
  CheckRunAnnotation
    { checkRunAnnotationPath        = testFilePath
    , checkRunAnnotationStartLine   = testLineNumber
    , checkRunAnnotationEndLine     = testLineNumber
    , checkRunAnnotationStartColumn = Nothing
    , checkRunAnnotationEndColumn   = Nothing
    , checkRunAnnotationLevel       = "failure"
    , checkRunAnnotationMessage     = testFullDescription
    , checkRunAnnotationTitle       = Just testDescription
    , checkRunAnnotationRawDetails  = Text.decodeUtf8 . ByteString.toStrict . encodePretty <$> testException
    }
-}
