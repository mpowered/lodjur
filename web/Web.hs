{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}
module Web
  ( runServer
  )
where

import qualified Data.Text                     as Text
import           Network.OAuth.OAuth2
import qualified Network.Wai.Handler.Warp      as Warp
import           Network.Wai.Handler.WebSockets
import           Network.WebSockets      hiding ( runServer )
import           Web.Spock               hiding ( static )
import           Web.Spock.Config

import           Auth.GitHub
import           Base
import           WebHook
import           Lodjur.Manager

homeAction :: Action ()
homeAction = do
  sess <- readSession
  text (Text.pack $ show sess)

welcomeAction :: Action ()
welcomeAction = redirect "/github/login"

ifLoggedIn :: Action () -> Action () -> Action ()
ifLoggedIn thenRoute elseRoute = readSession >>= \case
  Session { currentUser = Just _ } -> thenRoute
  _ -> elseRoute

runServer :: Int -> Env -> OAuth2 -> IO ()
runServer port env githubOauth = do
  cfg    <- defaultSpockCfg emptySession PCNoDatabase env
  lodjur <- spockAsApp (spock cfg app)
  wsmgr  <- newManager
  Warp.run port $ websocketsOr opts (managerWebServerApp wsmgr) lodjur
 where
  opts = defaultConnectionOptions
  app  = do
    -- Middleware
    -- middleware (staticPolicy (redirectStatic staticBase))

    -- Auth
    authRoutes githubOauth

    -- Webhook
    post "/github/webhook" webhookAction

    -- Routes
    get "/" (ifLoggedIn homeAction welcomeAction)

    -- requireLoggedIn $ do
    --   get "/jobs" getDeploymentJobsAction
    --   post "/jobs" newDeployAction
    --   get ("jobs" <//> var)               showJobAction
    --   get ("jobs" <//> var <//> "output") streamOutputAction
    --   get ("jobs" <//> var <//> "result" <//> var) getResultAction
