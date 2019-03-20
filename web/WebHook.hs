{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TypeFamilies          #-}
module WebHook
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
import qualified Data.HashSet                  as Set
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

import           Base
import           WebHook.Events

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
    "completed"     -> updateCheckSuite suite
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
    "completed"         -> updateCheckRun run
    _                   -> raise "Unknown check_run action received"

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

    void $ Q.push Msg.checkRequestedQueue [GH.eventCheckSuiteId suite]

checkRun :: GH.EventCheckRun -> Action ()
checkRun run = do
  Env {..} <- getState
  let suite  = GH.eventCheckRunCheckSuite run

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

    Db.modify (Db.checkSuiteKeyFromId $ GH.eventCheckSuiteId suite) $
      \cs -> cs
        { Db.checkRuns  = Set.insert (GH.eventCheckRunId run) (Db.checkRuns cs)
        } :: Db.CheckSuite

    void $ Q.push Msg.runRequestedQueue [GH.eventCheckRunId run]

updateCheckSuite :: GH.EventCheckSuite -> Action ()
updateCheckSuite suite = do
  Env {..} <- getState

  liftIO $ Redis.runRedis envRedisConn $ do
    Db.modify (Db.checkSuiteKeyFromId $ GH.eventCheckSuiteId suite) $
      \cs -> cs
        { Db.status     = GH.eventCheckSuiteStatus suite
        , Db.conclusion = GH.eventCheckSuiteConclusion suite
        } :: Db.CheckSuite
    when (GH.eventCheckSuiteStatus suite == GH.Completed) $
      void $ Q.move Msg.checkInProgressQueue Msg.checkCompletedQueue (GH.eventCheckSuiteId suite)

updateCheckRun :: GH.EventCheckRun -> Action ()
updateCheckRun run = do
  Env {..} <- getState

  liftIO $ Redis.runRedis envRedisConn $
    if GH.eventCheckRunStatus run == GH.Completed
    then
      Db.modify (Db.checkRunKeyFromId $ GH.eventCheckRunId run) $
        \cs -> cs
          { Db.status      = GH.eventCheckRunStatus run
          , Db.conclusion  = GH.eventCheckRunConclusion run
          , Db.completedAt = GH.eventCheckRunCompletedAt run
          } :: Db.CheckRun
    else
      Db.modify (Db.checkRunKeyFromId $ GH.eventCheckRunId run) $
        \cs -> cs
          { Db.status     = GH.eventCheckRunStatus run
          , Db.conclusion = GH.eventCheckRunConclusion run
          } :: Db.CheckRun

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
