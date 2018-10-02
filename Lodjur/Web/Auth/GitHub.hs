{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
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
import           URI.ByteString.QQ
import           Web.Spock

import           Lodjur.Web.Base

oauth2 :: OAuth2
oauth2 = OAuth2
  { oauthClientId = ""
  , oauthClientSecret = ""
  , oauthOAuthorizeEndpoint = [uri|https://github.com/login/oauth/authorize|]
  , oauthAccessTokenEndpoint = [uri|https://github.com/login/oauth/access_token|]
  , oauthCallback = Just [uri|http://localhost:4000/auth/github/callback|]
  }

startGithubAuthentication :: Action ()
startGithubAuthentication =
  let
    scope   = "read:org"
    state   = "foobar"
    params' = [("scope", scope), ("state", state), ("allow_signup", "false")]
    url     = appendQueryParams params' (authorizationUrl oauth2)
  in
    redirect (Text.decodeUtf8 (serializeURIRef' url))

exchangeCode :: Action ()
exchangeCode = do
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

authRoutes :: App ()
authRoutes = do
  -- Auth
  get ("auth" <//> "github" <//> "login")    startGithubAuthentication
  get ("auth" <//> "github" <//> "callback") exchangeCode
  get ("auth" <//> "github" <//> "logout")   clearSession
