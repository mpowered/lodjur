{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Lodjur.Web.Auth.GitHub where

import           Control.Monad.IO.Class
import qualified Data.Text                 as Text
import qualified Data.Text.Encoding        as Text
import           Network.HTTP.Client
import           Network.HTTP.Client.TLS
import           Network.HTTP.Types.Status
import           Network.OAuth.OAuth2
import           URI.ByteString
import           Web.Spock

import           Lodjur.Web.Base

startGithubAuthentication :: OAuth2 -> Action ()
startGithubAuthentication oauth2 =
  let
    scope   = "read:org"
    state   = "foobar"
    params' = [("scope", scope), ("state", state), ("allow_signup", "false")]
    url     = appendQueryParams params' (authorizationUrl oauth2)
  in
    redirect (Text.decodeUtf8 (serializeURIRef' url))

exchangeCode :: OAuth2 -> Action ()
exchangeCode oauth2 = do
  code   <- param' "code"
  result <- liftIO $ do
    manager <- newManager tlsManagerSettings
    fetchAccessToken manager oauth2 (ExchangeToken code)
  case result of
    Left err -> do
      setStatus status400
      text (Text.pack (show err))
    Right OAuth2Token {..} ->
      redirect "/"

clearSession :: Action ()
clearSession = redirect ""

authRoutes :: OAuth2 -> App ()
authRoutes oauth2 = do
  -- Auth
  get ("auth" <//> "github" <//> "login")    (startGithubAuthentication oauth2)
  get ("auth" <//> "github" <//> "callback") (exchangeCode oauth2)
  get ("auth" <//> "github" <//> "logout")   clearSession
