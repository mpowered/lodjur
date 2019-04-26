{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

module WebHook where

import           Control.Monad
import           Control.Monad.IO.Class         ( liftIO )
import           Servant

import           Lodjur.Core
import           Lodjur.GitHub
import           Lodjur.GitHub.Events
import           Lodjur.GitHub.Payload
import           Lodjur.GitHub.Webhook
import           Lodjur.Job
import           Types

type Webhook
    = GitHubCheckEvent '[ 'WebhookCheckSuiteEvent ] :> GitHubSignedReqBody '[JSON] (EventWithHookRepo CheckSuiteEvent) :> Post '[JSON] ()
 :<|> GitHubCheckEvent '[ 'WebhookCheckRunEvent   ] :> GitHubSignedReqBody '[JSON] (EventWithHookRepo CheckRunEvent  ) :> Post '[JSON] ()

webhook :: ServerT Webhook AppM
webhook = checkSuiteEvent :<|> checkRunEvent

checkSuiteEvent :: RepoWebhookCheckEvent -> ((), EventWithHookRepo CheckSuiteEvent) -> AppM ()
checkSuiteEvent _ (_, e) = do
  let CheckSuiteEvent {..} = eventOf e
      action = evCheckSuiteAction
      repo   = evCheckSuiteRepository
      suite  = evCheckSuiteCheckSuite

  validateApp (whCheckSuiteApp suite)

  case action of
    "requested"   -> checkRequested suite repo
    "rerequested" -> checkRequested suite repo
    "completed"   -> return ()
    _             -> throwError err422 { errBody = "Unknown check_suite action received" }

checkRunEvent :: RepoWebhookCheckEvent -> ((), EventWithHookRepo CheckRunEvent) -> AppM ()
checkRunEvent _ (_, e) = do
  let CheckRunEvent {..} = eventOf e
      action = evCheckRunAction
      run    = evCheckRunCheckRun
      suite  = whCheckRunCheckSuite run

  validateApp (whCheckSuiteApp suite)

  case action of
    "created"          -> return () -- return ()
    "rerequested"      -> return () -- ignoreEvent   -- TODO: rerun
    "requested_action" -> return () -- ignoreEvent
    "completed"        -> return () -- return ()
    _                  -> throwError err422 { errBody = "Unknown check_run action received" }

checkRequested :: HookCheckSuite -> HookRepository -> AppM ()
checkRequested HookCheckSuite{..} HookRepository{..} = do
  owner <- either (maybe (throwError err412 { errBody = "Can't locate repository owner login name" }) return . whSimplUserLogin)
                  (return . whUserLogin)
                  whRepoOwner
  let HookHeadCommit{..} = whCheckSuiteHeadCommit
      commit =
        GitHubCommit
          { ghcSha = whCheckSuiteHeadSha
          , ghcOwner = owner
          , ghcRepo = whRepoName
          , ghcBranch = Just whCheckSuiteHeadBranch
          , ghcMessage = Just whCommitMessage
          , ghcAuthor = Just (whSimplUserName whAuthor)
          , ghcAuthorEmail = Just (whSimplUserEmail whAuthor)
          , ghcCommitter = Just (whSimplUserName whCommitter)
          , ghcCommitterEmail = Just (whSimplUserEmail whCommitter)
          , ghcTimestamp = Just whTimestamp
          }
  core <- getEnv envCore
  liftIO $ submit core Nothing (Request "build" commit (Build { doCheck = True}))

validateApp :: HookApp -> AppM ()
validateApp a = do
  appId <- getEnv envGithubAppId
  unless (appId == whAppId a) $
    throwError err412 { errBody = "Incorrect AppId" }
