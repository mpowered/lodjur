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

import qualified Lodjur.Database               as Db
import qualified Lodjur.Database.CheckRun      as Db
import qualified Lodjur.Database.CheckSuite    as Db
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
  Env {..}    <- getState

  _delivery   <- header "X-GitHub-Delivery"
  githubEvent <- requiredHeader "X-GitHub-Event"
  event       <- secureJsonData
  case githubEvent of
    "check_suite" -> checkSuiteEvent =<< parseEvent event
    "check_run"   -> checkRunEvent =<< parseEvent event
    _             -> ignoreEvent
  text "Event received"
 where
  parseEvent e = case fromJSON e of
    Success a   -> return a
    Error   err -> raise $ "event - no parse: " <> Text.pack err

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
    "completed"   -> ignoreEvent
    _             -> raise "Unknown check_suite action received"

checkRunEvent :: CheckRunEvent -> Action ()
checkRunEvent CheckRunEvent {..} = do
  let action = checkRunEventAction
      run    = checkRunEventCheckRun
      suite  = GH.eventCheckRunCheckSuite run
      app    = GH.eventCheckSuiteApp suite

  validateApp app

  case action of
    "created"          -> ignoreEvent
    "rerequested"      -> ignoreEvent   -- TODO: rerun
    "requested_action" -> ignoreEvent
    "completed"        -> ignoreEvent
    _                  -> raise "Unknown check_run action received"

data WebHookError = GithubError GH.Error

type WebHook = ExceptT WebHookError (ReaderT Env IO)

runWebHook :: Env -> WebHook a -> IO (Either WebHookError a)
runWebHook env a = runReaderT (runExceptT a) env

eitherIO :: MonadIO m => (a -> e) -> IO (Either a b) -> ExceptT e m b
eitherIO f a = liftIO a >>= hoistEither . first f

checkrunGHtoDB :: GH.CheckRun -> Db.CheckRun
checkrunGHtoDB GH.CheckRun{..} =
  Db.CheckRun
    { Db.checkrunId           = GH.untagId checkRunCheckSuiteId
    , Db.checkrunName         = GH.untagName checkRunName
    , Db.checkrunStatus       = Db.DbEnum checkRunStatus
    , Db.checkrunConclusion   = Db.DbEnum <$> checkRunConclusion
    , Db.checkrunStartedAt    = checkRunStartedAt
    , Db.checkrunCompletedAt  = checkRunCompletedAt
    , Db.checkrunCheckSuite   = Db.CheckSuiteKey (GH.untagId checkRunCheckSuiteId)
    }

createCheckRun :: Msg.Source -> GH.Name GH.CheckRun -> WebHook GH.CheckRun
createCheckRun Msg.Source{..} name = do
  Env{..} <- ask
  tok <- liftIO $ ensureToken envGithubInstallationAccessToken
  run <- eitherIO GithubError $ GH.createCheckRun (GH.OAuth tok) (GH.simpleOwnerLogin owner) repo $
    (GH.newCheckRun name sha)
      { GH.newCheckRunStatus = Just GH.Queued
      }
  liftIO $ withResource envDbPool $ \conn -> Db.upsertCheckRun conn (checkrunGHtoDB run)
  return run

updateCheckRun :: Msg.Source -> GH.Id GH.CheckRun -> GH.UpdateCheckRun -> WebHook GH.CheckRun
updateCheckRun Msg.Source{..} runid run = do
  Env{..} <- ask
  tok <- liftIO $ ensureToken envGithubInstallationAccessToken
  urun <- eitherIO GithubError $ GH.updateCheckRun (GH.OAuth tok) (GH.simpleOwnerLogin owner) repo runid run
  liftIO $ withResource envDbPool $ \conn -> Db.upsertCheckRun conn (checkrunGHtoDB urun)
  return urun

checkRun :: GH.Name GH.CheckRun -> Msg.Request -> WebHook ()
checkRun name req = do
  env@Env{..} <- ask
  run <- createCheckRun src name
  rep <- eitherIO id $ Mgr.withClient envWorkManager $ \client -> do
    Mgr.sendRequest req client
    iprun <- inprogress env (GH.checkRunId run)
    case iprun of
      Left e -> Mgr.cancelRequest client >> return (Left e)
      Right _ -> Right <$> Mgr.waitReply client
  case rep of
    Msg.Completed concl output -> do
      now <- liftIO getCurrentTime
      _crun <- updateCheckRun src (GH.checkRunId run) $
        GH.emptyUpdateCheckRun
          { GH.updateCheckRunStatus       = Just GH.Completed
          , GH.updateCheckRunConclusion   = Just concl
          , GH.updateCheckRunOutput       = output
          , GH.updateCheckRunCompletedAt  = Just now
          }
      return ()
    Msg.Disconnected -> do
      now <- liftIO getCurrentTime
      _crun <- updateCheckRun src (GH.checkRunId run) $
        GH.emptyUpdateCheckRun
          { GH.updateCheckRunStatus       = Just GH.Completed
          , GH.updateCheckRunConclusion   = Just GH.Cancelled
          , GH.updateCheckRunCompletedAt  = Just now
          }
      return ()

  where
    src = Msg.src req
    inprogress env runid = do
      now <- getCurrentTime
      runWebHook env $ updateCheckRun src runid $
        GH.emptyUpdateCheckRun
          { GH.updateCheckRunStatus    = Just GH.InProgress
          , GH.updateCheckRunStartedAt = Just now
          }

checkRequested :: GH.EventCheckSuite -> GH.Repo -> Action ()
checkRequested GH.EventCheckSuite{..} GH.Repo{..} = do
  env@Env {..} <- getState
  start <- liftIO getCurrentTime
  runQuery $ \conn ->
    Db.upsertCheckSuite conn
      Db.CheckSuite
      { checksuiteId              = GH.untagId eventCheckSuiteId
      , checksuiteRepositoryOwner = GH.untagName (GH.simpleOwnerLogin repoOwner)
      , checksuiteRepositoryName  = GH.untagName repoName
      , checksuiteHeadSha         = GH.untagSha  eventCheckSuiteHeadSha
      , checksuiteStatus          = Db.DbEnum eventCheckSuiteStatus
      , checksuiteConclusion      = Db.DbEnum <$> eventCheckSuiteConclusion
      , checksuiteStartedAt       = Just start
      , checksuiteCompletedAt     = Nothing
      }

  let src = Msg.Source eventCheckSuiteHeadSha repoOwner repoName

  _ <- liftIO $ runWebHook env $ checkRun "build" (Msg.Build src)
  return ()

-- checkRun :: GH.EventCheckRun -> Action ()
-- checkRun run = do
--   Env {..} <- getState
--   return ()
  -- let suite  = GH.eventCheckRunCheckSuite run

  -- liftIO $ Redis.runRedis envRedisConn $ do
  --   Db.put_ (Db.checkRunKeyFromId $ GH.eventCheckRunId run)
  --     Db.CheckRun
  --       { checkSuiteId = GH.eventCheckSuiteId suite
  --       , name         = GH.eventCheckRunName run
  --       , headSha      = GH.eventCheckRunHeadSha run
  --       , status       = GH.eventCheckRunStatus run
  --       , conclusion   = GH.eventCheckRunConclusion run
  --       , startedAt    = GH.eventCheckRunStartedAt run
  --       , completedAt  = GH.eventCheckRunCompletedAt run
  --       , externalId   = GH.eventCheckRunExternalId run
  --       , output       = GH.eventCheckRunOutput run
  --       }

  --   Db.modify (Db.checkSuiteKeyFromId $ GH.eventCheckSuiteId suite) $
  --     \cs -> cs
  --       { Db.checkRuns  = Set.insert (GH.eventCheckRunId run) (Db.checkRuns cs)
  --       } :: Db.CheckSuite

  --   void $ Q.push Msg.runRequestedQueue [GH.eventCheckRunId run]

updateCheckSuite :: GH.EventCheckSuite -> Action ()
updateCheckSuite suite = do
  Env {..} <- getState
  return ()

  -- liftIO $ Redis.runRedis envRedisConn $ do
  --   Db.modify (Db.checkSuiteKeyFromId $ GH.eventCheckSuiteId suite) $
  --     \cs -> cs
  --       { Db.status     = GH.eventCheckSuiteStatus suite
  --       , Db.conclusion = GH.eventCheckSuiteConclusion suite
  --       } :: Db.CheckSuite
  --   when (GH.eventCheckSuiteStatus suite == GH.Completed) $
  --     void $ Q.move Msg.checkInProgressQueue Msg.checkCompletedQueue (GH.eventCheckSuiteId suite)

-- updateCheckRun :: GH.EventCheckRun -> Action ()
-- updateCheckRun run = do
--   Env {..} <- getState
--   return ()

  -- liftIO $ Redis.runRedis envRedisConn $
  --   if GH.eventCheckRunStatus run == GH.Completed
  --   then
  --     Db.modify (Db.checkRunKeyFromId $ GH.eventCheckRunId run) $
  --       \cs -> cs
  --         { Db.status      = GH.eventCheckRunStatus run
  --         , Db.conclusion  = GH.eventCheckRunConclusion run
  --         , Db.completedAt = GH.eventCheckRunCompletedAt run
  --         } :: Db.CheckRun
  --   else
  --     Db.modify (Db.checkRunKeyFromId $ GH.eventCheckRunId run) $
  --       \cs -> cs
  --         { Db.status     = GH.eventCheckRunStatus run
  --         , Db.conclusion = GH.eventCheckRunConclusion run
  --         } :: Db.CheckRun

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

workerRequest :: Msg.Request -> (Msg.Reply -> IO ()) -> Action (Async ())
workerRequest req action = do
  Env {..} <- getState
  liftIO $ async $ do
    rep <- Mgr.request envWorkManager req
    action rep
