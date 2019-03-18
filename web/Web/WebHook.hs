{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
module Web.WebHook
  ( webhookAction
  )
where

import           Control.Monad
import           Control.Monad.IO.Class        (liftIO)
import           Control.Monad.Reader
import           Crypto.Hash
import           Crypto.MAC.HMAC
import           Data.Aeson                    hiding (json)
import qualified Data.ByteString.Base16        as Base16
import           Data.Text                     (Text)
import qualified Data.Text                     as Text
import qualified Data.Text.Encoding            as Text
import           Data.Time.Clock               (getCurrentTime)
import           Network.HTTP.Types.Status
import           Web.Spock

import qualified Database.Redis                as Redis
import qualified Database.Redis.Queue          as Q

import qualified Lodjur.Database               as Db
import qualified Lodjur.Database.Checks        as Db
import qualified Lodjur.Messages               as Msg

import           Web.Base
import           Web.WebHook.Events

import qualified GitHub                        as GH
import qualified GitHub.Extra                  as GH

ignoreEvent :: Action ()
ignoreEvent = text "Event ignored"

webhookAction :: Action ()
webhookAction = do
  Env {..} <- getState

  _delivery   <- header "X-GitHub-Delivery"
  githubEvent <- requiredHeader "X-GitHub-Event"
  _now        <- liftIO getCurrentTime
  event       <- secureJsonData
  case githubEvent of
    "check_suite" -> checkSuiteEvent =<< parseEvent event
    "check_run"   -> checkRunEvent =<< parseEvent event
    _             -> ignoreEvent
  text "Event received"
  where
    parseEvent e =
      case fromJSON e of
        Success a -> return a
        Error err -> raise $ "event - no parse: " <> Text.pack err

checkSuiteEvent :: CheckSuiteEvent -> Action ()
checkSuiteEvent CheckSuiteEvent {..} = do
  let action = checkSuiteEventAction
      repo   = checkSuiteEventRepository
      suite  = checkSuiteEventCheckSuite
      app    = GH.eventCheckSuiteApp suite

  Env {..} <- getState

  unless (envGithubAppId == GH.untagId (GH.appId app)) $
    text "Event ignored, different AppId"

  case action of
    "requested"     -> checkRequested suite repo
    "rerequested"   -> checkRequested suite repo
    "completed"     -> ignoreEvent
    _               -> raise "Unknown check_suite action received"

checkRunEvent :: CheckRunEvent -> Action ()
checkRunEvent CheckRunEvent {..} = do
  let action = checkRunEventAction
      run    = checkRunEventCheckRun
      suite  = GH.eventCheckRunCheckSuite run
      app    = GH.eventCheckSuiteApp suite

  Env {..} <- getState

  unless (envGithubAppId == GH.untagId (GH.appId app)) $
    text "Event ignored, different AppId"

  case action of
    "created"           -> checkRun run
    "rerequested"       -> checkRun run
    "requested_action"  -> ignoreEvent
    "completed"         -> ignoreEvent
    _                   -> raise "Unknown check_run action received"

repoToRef :: GH.Repo -> GH.RepoRef
repoToRef repo = GH.RepoRef { repoRefOwner = GH.repoOwner repo, repoRefRepo = GH.repoName repo }

checkRequested :: GH.EventCheckSuite -> GH.Repo -> Action ()
checkRequested suite repo = do
  Env {..} <- getState

  liftIO $ Redis.runRedis envRedisConn $ do
    Db.put_ (Db.checkSuiteKeyFromId $ GH.eventCheckSuiteId suite)
      Db.CheckSuite
        { headBranch = GH.eventCheckSuiteHeadBranch suite
        , headSha    = GH.eventCheckSuiteHeadSha suite
        , status     = GH.eventCheckSuiteStatus suite
        , conclusion = GH.eventCheckSuiteConclusion suite
        , repository = repo
        , checkRuns  = mempty
        }

    jobids <-
      Q.push Msg.workersQueue
        [ Msg.CheckRequested
          { repo = repoToRef repo
          , headSha = GH.eventCheckSuiteHeadSha suite
          , checkSuiteId = GH.eventCheckSuiteId suite
          }
        ]
    Q.setTtl jobids (1*60*60)

checkRun :: GH.EventCheckRun -> Action ()
checkRun run = do
  let suite  = GH.eventCheckRunCheckSuite run

  Env {..} <- getState

  liftIO $ Redis.runRedis envRedisConn $ do
    Db.put_ (Db.checkRunKeyFromId $ GH.eventCheckRunId run)
      Db.CheckRun
        { checkSuiteId = GH.eventCheckSuiteId suite
        , name         = GH.eventCheckRunName run
        , headSha      = GH.eventCheckRunHeadSha run
        , status       = GH.eventCheckRunStatus run
        , conclusion   = GH.eventCheckRunConclusion run
        , startedAt    = GH.eventCheckRunStartedAt run
        , completedAt  = GH.eventCheckRunCompletedAt run
        , output       = GH.eventCheckRunOutput run
        }

raise :: MonadIO m => Text -> ActionCtxT ctx m b
raise msg = do
  setStatus status400
  text msg

secureJsonData :: FromJSON a => Action a
secureJsonData = do
  key     <- envGithubSecretToken <$> getState
  message <- body
  xhubsig <- requiredHeader "X-HUB-SIGNATURE"
  sig     <- maybe (raise "Github X-HUB-SIGNATURE didn't start with 'sha1='") return
                     (Text.stripPrefix "sha1=" xhubsig)
  digest  <- maybe (raise "Invalid SHA1 digest sent in X-HUB-SIGNATURE") return
                  (digestFromByteString $ fst $ Base16.decode $ Text.encodeUtf8 sig)
  unless (hmac key message == HMAC (digest :: Digest SHA1))
    $ raise "Signatures don't match"
  either
    (\e ->
      raise
        $  "jsonData - no parse: "
        <> Text.pack e
        <> ". Data was:"
        <> Text.decodeUtf8 message
    )
    return
    (eitherDecodeStrict message)

requiredHeader :: MonadIO m => Text -> ActionCtxT ctx m Text
requiredHeader hdr =
  header hdr >>= \case
    Just bs -> return bs
    Nothing -> raise $ "Missing required header " <> hdr
