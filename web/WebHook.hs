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

import           Control.Concurrent.Async
import           Control.Error
import           Control.Monad
import           Control.Monad.IO.Class         ( liftIO )
import           Control.Monad.Reader
import           Crypto.Hash
import           Crypto.MAC.HMAC
import           Data.Aeson              hiding ( json )
import           Data.Bifunctor
import qualified Data.ByteString.Base16        as Base16
import           Data.Pool
import           Data.Text                      ( Text )
import qualified Data.Text                     as Text
import qualified Data.Text.Encoding            as Text
import           Data.Time.Clock                ( getCurrentTime )
import           Network.HTTP.Types.Status
import           Web.Spock

import qualified Lodjur.Core                   as Core
import qualified Lodjur.Database               as Db
import qualified Lodjur.Database.CheckRun      as Db
import qualified Lodjur.Database.CheckSuite    as Db
import qualified Lodjur.Database.Event         as Db
import           Lodjur.GitHub
import qualified Lodjur.Manager                as Mgr
import qualified Lodjur.Manager.Messages       as Msg

import           Base
import           WebHook.Events

import qualified GitHub                        as GH
import qualified GitHub.Extra                  as GH
import qualified GitHub.Endpoints.Checks       as GH

ignoreEvent :: Action ()
ignoreEvent = text "Event ignored"

webhookAction :: Action ()
webhookAction = do
  now         <- liftIO getCurrentTime
  delivery    <- header "X-GitHub-Delivery"
  githubEvent <- requiredHeader "X-GitHub-Event"
  event       <- secureJsonData
  runQuery $ \conn -> Db.insertEventsE conn
    [ Db.Event  { Db.eventId = Db.default_
                , Db.eventSource = Db.val_ "GitHub"
                , Db.eventDelivery = Db.val_ delivery
                , Db.eventType = Db.val_ githubEvent
                , Db.eventCreatedAt = Db.val_ now
                , Db.eventData = Db.val_ event
                }
    ]
  case githubEvent of
    "check_suite" -> checkSuiteEvent =<< parseEvent event
    "check_run"   -> checkRunEvent =<< parseEvent event
    _             -> ignoreEvent
  text "Event received"
 where
  parseEvent e = case fromJSON e of
    Success a -> return a
    Error   x -> raise $ "event - no parse: " <> Text.pack x

checkSuiteEvent :: CheckSuiteEvent -> Action ()
checkSuiteEvent CheckSuiteEvent {..} = do
  let action = checkSuiteEventAction
      repo   = checkSuiteEventRepository
      suite  = checkSuiteEventCheckSuite
      app    = GH.eventCheckSuiteApp suite

  validateApp app

  case action of
    "requested"   -> checkRequested suite repo
    "rerequested" -> checkRequested suite repo
    "completed"   -> updateCheckSuite suite repo
    _             -> raise "Unknown check_suite action received"

checkRunEvent :: CheckRunEvent -> Action ()
checkRunEvent CheckRunEvent {..} = do
  let action = checkRunEventAction
      run    = checkRunEventCheckRun
      suite  = GH.eventCheckRunCheckSuite run
      app    = GH.eventCheckSuiteApp suite

  validateApp app

  case action of
    "created"          -> updateCheckRun run
    "rerequested"      -> ignoreEvent   -- TODO: rerun
    "requested_action" -> ignoreEvent
    "completed"        -> updateCheckRun run
    _                  -> raise "Unknown check_run action received"

checkRequested :: GH.EventCheckSuite -> GH.Repo -> Action ()
checkRequested GH.EventCheckSuite{..} GH.Repo{..} = do
  Env{..} <- getState
  let src = Msg.Source eventCheckSuiteHeadSha repoOwner repoName

  r <- liftIO $ Core.request envCore (Msg.Build "build" src)
  case r of
    Left e -> text $ "Failed to queue run: " <> Text.pack (show e)
    Right () -> return ()

updateCheckSuite :: GH.EventCheckSuite -> GH.Repo -> Action ()
updateCheckSuite GH.EventCheckSuite{..} GH.Repo{..} = runQuery $ \conn ->
  Db.upsertCheckSuite conn
    Db.CheckSuite
    { checksuiteId              = GH.untagId eventCheckSuiteId
    , checksuiteRepositoryOwner = GH.untagName (GH.simpleOwnerLogin repoOwner)
    , checksuiteRepositoryName  = GH.untagName repoName
    , checksuiteHeadSha         = GH.untagSha eventCheckSuiteHeadSha
    , checksuiteStatus          = Db.DbEnum eventCheckSuiteStatus
    , checksuiteConclusion      = Db.DbEnum <$> eventCheckSuiteConclusion
    }

updateCheckRun :: GH.EventCheckRun -> Action ()
updateCheckRun GH.EventCheckRun{..} = runQuery $ \conn ->
  Db.upsertCheckRun conn
    Db.CheckRun
    { checkrunId                = GH.untagId eventCheckRunId
    , checkrunCheckSuite        = Db.CheckSuiteKey (GH.untagId $ GH.eventCheckSuiteId eventCheckRunCheckSuite)
    , checkrunName              = GH.untagName eventCheckRunName
    , checkrunStatus            = Db.DbEnum eventCheckRunStatus
    , checkrunConclusion        = Db.DbEnum <$> eventCheckRunConclusion
    , checkrunStartedAt         = eventCheckRunStartedAt
    , checkrunCompletedAt       = eventCheckRunCompletedAt
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
  sig     <- maybe (raise "Github X-HUB-SIGNATURE didn't start with 'sha1='")
                   return
                   (Text.stripPrefix "sha1=" xhubsig)
  digest <- maybe
    (raise "Invalid SHA1 digest sent in X-HUB-SIGNATURE")
    return
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
requiredHeader hdr = header hdr >>= \case
  Just bs -> return bs
  Nothing -> raise $ "Missing required header " <> hdr

validateApp :: GH.App -> Action ()
validateApp app = do
  Env {..} <- getState

  unless (envGithubAppId == GH.untagId (GH.appId app))
    $ text "Event ignored, different AppId"

-- workerRequest :: Msg.Request -> (Msg.Reply -> IO ()) -> Action (Async ())
-- workerRequest req action = do
--   Env {..} <- getState
--   liftIO $ async $ do
--     rep <- Mgr.request envWorkManager req
--     action rep
