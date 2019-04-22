{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

module WebHook where

import           Servant
import           Types
import           Lodjur.GitHub.Events
import           Lodjur.GitHub.Webhook

type Webhook
    = GitHubCheckEvent '[ 'WebhookCheckSuiteEvent ] :> GitHubSignedReqBody '[JSON] (EventWithHookRepo CheckSuiteEvent) :> Post '[JSON] ()
 :<|> GitHubCheckEvent '[ 'WebhookCheckRunEvent   ] :> GitHubSignedReqBody '[JSON] (EventWithHookRepo CheckRunEvent  ) :> Post '[JSON] ()

webhook :: ServerT Webhook AppM
webhook = checkSuiteEvent :<|> checkRunEvent

checkSuiteEvent :: RepoWebhookCheckEvent -> ((), EventWithHookRepo CheckSuiteEvent) -> AppM ()
checkSuiteEvent _ _ = return ()

checkRunEvent :: RepoWebhookCheckEvent -> ((), EventWithHookRepo CheckRunEvent) -> AppM ()
checkRunEvent _ _ = return ()

{-
  ( webhookAction
  )
where

import           Control.Monad
import           Control.Monad.IO.Class         ( liftIO )
import           Control.Monad.Reader
import           Crypto.Hash
import           Crypto.MAC.HMAC
import           Data.Aeson              hiding ( json )
import qualified Data.ByteString.Base16        as Base16
import           Data.Text                      ( Text )
import qualified Data.Text                     as Text
import qualified Data.Text.Encoding            as Text
import           Network.HTTP.Types.Status
import           Web.Spock

import qualified Lodjur.Core                   as Core
import qualified Lodjur.GitHub                 as GH
import qualified Lodjur.Job                    as Job

import           Base

ignoreEvent :: Action ()
ignoreEvent = text "Event ignored"

webhookAction :: Action ()
webhookAction = do
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
    Success a -> return a
    Error   x -> raise $ "event - no parse: " <> Text.pack x

checkSuiteEvent :: CheckSuiteEvent -> Action ()
checkSuiteEvent CheckSuiteEvent {..} = do
  let action = checkSuiteEventAction
      repo   = checkSuiteEventRepository
      suite  = checkSuiteEventCheckSuite
      app    = GH.eventCheckSuiteApp suite

  validateApp app

  -- updateCheckSuite suite repo

  case action of
    "requested"   -> checkRequested suite repo
    "rerequested" -> checkRequested suite repo
    "completed"   -> return ()
    _             -> raise "Unknown check_suite action received"

checkRunEvent :: CheckRunEvent -> Action ()
checkRunEvent CheckRunEvent {..} = do
  let action = checkRunEventAction
      run    = checkRunEventCheckRun
      suite  = GH.eventCheckRunCheckSuite run
      app    = GH.eventCheckSuiteApp suite

  validateApp app

  -- updateCheckRun run

  case action of
    "created"          -> return ()
    "rerequested"      -> ignoreEvent   -- TODO: rerun
    "requested_action" -> ignoreEvent
    "completed"        -> return ()
    _                  -> raise "Unknown check_run action received"

checkRequested :: GH.EventCheckSuite -> GH.Repo -> Action ()
checkRequested GH.EventCheckSuite{..} GH.Repo{..} = do
  Env{..} <- getState
  let src = GH.Source eventCheckSuiteHeadSha repoOwner repoName (Just eventCheckSuiteHeadBranch) eventCheckSuiteHeadCommit
  liftIO $ Core.submit envCore Nothing (Job.Request "build" src (Job.Build { doCheck = True}))

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

-}
