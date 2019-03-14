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

import           Database.Beam
import qualified Database.Redis                as Redis
import qualified Database.Redis.Queue          as Q

-- import qualified Lodjur.Database               as DB
-- import qualified Lodjur.Database.CheckRun      as DB
-- import qualified Lodjur.Database.CheckSuite    as DB
-- import qualified Lodjur.Database.Event         as DB
import qualified Lodjur.Messages               as Msg

import           Web.Base
import           Web.WebHook.Events

import           GitHub
-- import           GitHub.Data.Id
-- import           GitHub.Data.Name
import           GitHub.Extra
-- import           GitHub.Endpoints.Apps        hiding (app)
-- import           GitHub.Endpoints.Checks

ignoreEvent :: Action ()
ignoreEvent = text "Event ignored"

webhookAction :: Action ()
webhookAction = do
  Env {..} <- getState

  delivery    <- header "X-GitHub-Delivery"
  githubEvent <- requiredHeader "X-GitHub-Event"
  now         <- liftIO getCurrentTime
  event       <- secureJsonData
  -- DB.withConn envDbPool $ \conn -> DB.insertEventsE conn
  --   [ DB.Event  { DB.eventId = default_
  --               , DB.eventSource = val_ "GitHub"
  --               , DB.eventDelivery = val_ delivery
  --               , DB.eventType = val_ githubEvent
  --               , DB.eventCreatedAt = val_ now
  --               , DB.eventData = val_ event
  --               }
  --   ]
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
      owner  = repoRefOwner repo
      suite  = checkSuiteEventCheckSuite
      app    = eventCheckSuiteApp suite

  Env {..} <- getState

  unless (envGithubAppId == untagId (appRefId app)) $
    text "Event ignored, different AppId"

  case action of
    "requested"     -> checkRequested suite owner repo
    "rerequested"   -> checkRequested suite owner repo
    "completed"     -> ignoreEvent
    _               -> raise "Unknown check_suite action received"

checkRunEvent :: CheckRunEvent -> Action ()
checkRunEvent CheckRunEvent {..} = do
  let action = checkRunEventAction
      run    = checkRunEventCheckRun
      suite  = eventCheckRunCheckSuite run
      app    = eventCheckSuiteApp suite
      repo   = checkRunEventRepository
      owner  = repoRefOwner repo

  Env {..} <- getState

  unless (envGithubAppId == untagId (appRefId app)) $
    text "Event ignored, different AppId"

  case action of
    "created"           -> checkRun run owner repo
    "rerequested"       -> checkRun run owner repo
    "requested_action"  -> ignoreEvent
    _                   -> raise "Unknown check_run action received"

checkRequested :: EventCheckSuite -> SimpleOwner -> RepoRef -> Action ()
checkRequested suite owner repo = do
  Env {..} <- getState

  liftIO $ Redis.runRedis envRedisConn $ do
    jobids <-
      Q.push Msg.workersQueue
        [ Msg.CheckRequested
          { repo = Msg.Repo (untagName $ simpleOwnerLogin owner) (untagName $ repoRefRepo repo)
          , headSha = eventCheckSuiteHeadSha suite
          , checkSuiteId = untagId (eventCheckSuiteId suite)
          }
        ]
    Q.setTtl jobids (1*60*60)

checkRun :: EventCheckRun -> SimpleOwner -> RepoRef -> Action ()
checkRun _run _owner _repo = return ()
-- checkRun run owner repo = do
--   let suite = eventCheckRunCheckSuite run

--   Env {..} <- getState

--   liftIO $ Redis.runRedis envRedisConn $ do
--     jobids <-
--       Q.push "checkruns"
--         [ Jobs.CheckRun
--           { jobRunId = untagId (eventCheckRunId run)
--           , jobName = untagName (eventCheckRunName run)
--           , jobRepo = Jobs.Repo (untagName $ simpleOwnerLogin owner) (untagName $ repoRefRepo repo)
--           , jobHeadSha = eventCheckRunHeadSha run
--           , jobSuiteId = untagId (eventCheckSuiteId suite)
--           }
--         ]
--     Q.setTtl jobids (1*60*60)

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
