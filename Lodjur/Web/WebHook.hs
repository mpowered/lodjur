{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
module Lodjur.Web.WebHook
  ( webhookAction
  )
where

import           Control.Concurrent
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
import           Data.Time.Clock               (UTCTime, getCurrentTime)
import           Data.Time.Clock.POSIX
import           Network.HTTP.Types.Status
import           Web.Spock
import qualified Web.JWT                       as JWT

import           Database.Beam

import qualified Lodjur.Database               as DB
import qualified Lodjur.Database.CheckRun      as DB
import qualified Lodjur.Database.CheckSuite    as DB
import qualified Lodjur.Database.Event         as DB
import qualified Lodjur.Jobs                   as Jobs
import qualified Lodjur.JobQueue               as Jobs

import           Lodjur.Web.Base
import           Lodjur.Web.WebHook.Events

import           GitHub
import           GitHub.Data.Id
import           GitHub.Extra
import           GitHub.Endpoints.Apps        hiding (app)
import           GitHub.Endpoints.Checks

webhookAction :: Action ()
webhookAction = do
  Env {..} <- getState

  delivery    <- header "X-GitHub-Delivery"
  githubEvent <- requiredHeader "X-GitHub-Event"
  now         <- liftIO getCurrentTime
  event       <- secureJsonData
  DB.withConn envDbPool $ \conn -> DB.insertEventsE conn
    [ DB.Event  { DB.eventId = default_
                , DB.eventSource = val_ "GitHub"
                , DB.eventDelivery = val_ delivery
                , DB.eventType = val_ githubEvent
                , DB.eventCreatedAt = val_ now
                , DB.eventData = val_ event
                }
    ]
  case githubEvent of
    "check_suite" -> checkSuiteEvent now =<< parseEvent event
    "check_run"   -> checkRunEvent =<< parseEvent event
    _             -> raise "Unknown event received"
  text "Event received"
  where
    parseEvent e =
      case fromJSON e of
        Success a -> return a
        Error err -> raise $ "event - no parse: " <> Text.pack err

checkSuiteEvent :: UTCTime -> CheckSuiteEvent -> Action ()
checkSuiteEvent now CheckSuiteEvent {..} = do
  let action = checkSuiteEventAction
      repo   = checkSuiteEventRepository
      owner  = repoRefOwner repo
      suite  = checkSuiteEventCheckSuite
      app    = eventCheckSuiteApp suite

  Env {..} <- getState

  unless (envGithubAppId == untagId (appRefId app)) $
    text "Event ignored, different AppId"

  case action of
    "requested"     -> checkRequested now suite owner repo
    "rerequested"   -> checkRequested now suite owner repo
    "completed"     -> return ()
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
    "created"           -> return ()
    "rerequested"       -> return ()
    "requested_action"  -> return ()
    _                   -> raise "Unknown check_run action received"

checkRequested :: UTCTime -> EventCheckSuite -> SimpleOwner -> RepoRef -> Action ()
checkRequested now suite owner repo = do
  Env {..} <- getState

  liftIO $ do
    jobids <-
      Jobs.enqueue envRedisConn "requested" (1*60*60)
        [ Jobs.CheckRequested
          { checkRepo = Jobs.Repo (untagName $ simpleOwnerLogin owner) (untagName $ repoRefRepo repo)
          , checkHeadSha = eventCheckSuiteHeadSha suite
          , checkSuiteId = untagId (eventCheckSuiteId suite)
          }
        ]
    mapM_ (\jobid -> Jobs.status envRedisConn jobid Jobs.Queued) jobids

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

createInstallationJWT :: Action Token
createInstallationJWT = do
  Env {..} <- getState
  now <- liftIO getPOSIXTime
  let claims = mempty { JWT.iss = JWT.stringOrURI (Text.pack $ show envGithubAppId)
                      , JWT.iat = JWT.numericDate now
                      , JWT.exp = JWT.numericDate (now + 600)
                      }
      jwt = JWT.encodeSigned envGithubAppSigner claims
  return $ Text.encodeUtf8 jwt

newInstallationAccessToken :: Action AccessToken
newInstallationAccessToken = do
  Env {..} <- getState
  tok <- createInstallationJWT
  result <- liftIO $
    executeRequestWithMgr envManager (Bearer tok) $
      createInstallationTokenR (Id envGithubInstallationId)
  case result of
    Left err ->
      raise $ "Unable to fetch installation access token: " <> Text.pack (show err)
    Right token ->
      return token

getInstallationAccessToken :: Action Auth
getInstallationAccessToken = do
  now <- liftIO getCurrentTime
  Env {..} <- getState
  let renew = do
        at <- newInstallationAccessToken
        liftIO $ putMVar envGithubInstallationAccessToken (Just at)
        return (asAuth at)
  tok <- liftIO $ takeMVar envGithubInstallationAccessToken
  case tok of
    Just at ->
      case accessTokenExpiresAt at of
        Just e ->
          if now >= e then renew else return (asAuth at)
        Nothing ->
          return (asAuth at)
    Nothing -> renew
  where
    asAuth = OAuth . accessToken
