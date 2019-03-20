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
import           Web.Spock                     hiding (static)
import           Web.Spock.Config

import           Auth.GitHub
import           Base
import           WebHook

homeAction :: Action ()
homeAction = do
  sess <- readSession
  text (Text.pack $ show sess)

welcomeAction :: Action ()
welcomeAction =
  redirect "/github/login"

ifLoggedIn :: Action () -> Action () -> Action ()
ifLoggedIn thenRoute elseRoute =
  readSession >>= \case
    Session{ currentUser = Just _ } -> thenRoute
    _ -> elseRoute

runServer
  :: Int
  -> Env
  -> OAuth2
  -> IO ()
runServer port env githubOauth = do
    cfg <- defaultSpockCfg emptySession PCNoDatabase env
    runSpock port (spock cfg app)
 where
  app = do
    -- Middleware
    -- middleware (staticPolicy (redirectStatic staticBase))

    -- Auth
    authRoutes githubOauth

    -- Routes
    get "/"     (ifLoggedIn homeAction welcomeAction)
    -- requireLoggedIn $ do
    --   get "/jobs" getDeploymentJobsAction
    --   post "/jobs" newDeployAction
    --   get ("jobs" <//> var)               showJobAction
    --   get ("jobs" <//> var <//> "output") streamOutputAction
    --   get ("jobs" <//> var <//> "result" <//> var) getResultAction
    post "/github/webhook" webhookAction

    -- Fallback
    -- hookAnyAll (const notFoundAction)
