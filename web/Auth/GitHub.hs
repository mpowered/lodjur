{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Auth.GitHub where

import           Control.Monad.Except
import           Control.Monad.IO.Class               (liftIO)
import           Data.Foldable                        (toList)
import qualified Data.Text                            as Text
import qualified Data.Text.Encoding                   as Text
import qualified Data.UUID                            as UUID
import qualified Data.UUID.V4                         as UUID
import qualified GitHub
import           Network.HTTP.Types.Status
import           Network.OAuth.OAuth2
import           URI.ByteString
import           Web.Spock

import           Base
import           User

-- For GitHub apps scopes aren't needed
startGithubAuthentication :: OAuth2 -> Action ()
startGithubAuthentication oauth2 = do
  state <- liftIO (UUID.toText <$> UUID.nextRandom)
  modifySession (\s -> s { oauthState = Just state })
  let
    params' = [("state", Text.encodeUtf8 state), ("allow_signup", "false")]
    url     = appendQueryParams params' (authorizationUrl oauth2)
  redirect (Text.decodeUtf8 (serializeURIRef' url))

checkError :: Action ()
checkError = do
  merr   <- param "error"
  case merr of
    Just err -> do
      allParams <- params
      setStatus status400
      text ("Error: " <> err <> ", Params: " <> Text.pack (show allParams))
    Nothing ->
      return ()

checkState :: Action ()
checkState = do
  savedState <- oauthState <$> readSession
  modifySession (\s -> s { oauthState = Nothing })
  state   <- param' "state"
  unless (Just state == savedState) $ do
    setStatus status400
    text "Bad OAuth state."

exchangeCode :: OAuth2 -> Action AccessToken
exchangeCode oauth2 = do
  Env {..} <- getState
  checkState
  code   <- param' "code"
  result <- liftIO $
    fetchAccessToken envHttpManager oauth2 (ExchangeToken code)
  case result of
    Left err -> do
      setStatus status400
      writeSession emptySession
      text (Text.pack (show err))
    Right OAuth2Token {..} ->
      return accessToken

getUser' :: AccessToken -> Action GitHub.User
getUser' accessToken = do
  githubResult <- getUser accessToken
  case githubResult of
    Left err -> do
      setStatus status400
      text ("Failed: " <> Text.pack (show err))
    Right user ->
      return user

loginAndRedirect :: GitHub.User -> Action ()
loginAndRedirect githubUser = do
  let user = User { userId = UserId (GitHub.untagName $ GitHub.userLogin githubUser) }
  continueTo' <- continueTo <$> readSession
  writeSession emptySession { currentUser = Just user }
  case continueTo' of
    Just relRef -> redirect (Text.decodeUtf8 (serializeURIRef' relRef))
    Nothing -> redirect "/"

authorizeAndLogin :: OAuth2 -> Action ()
authorizeAndLogin oauth2 = do
  checkError
  accessToken <- exchangeCode oauth2
  githubUser <- getUser' accessToken
  loginAndRedirect githubUser

toOAuth :: AccessToken -> GitHub.Auth
toOAuth (AccessToken accessToken) =
  GitHub.OAuth (Text.encodeUtf8 accessToken)

getUser :: AccessToken -> Action (Either GitHub.Error GitHub.User)
getUser at = do
  Env {..} <- getState
  liftIO $
    GitHub.executeRequestWithMgr
      envHttpManager
      (toOAuth at)
      GitHub.userInfoCurrentR

clearSession :: Action ()
clearSession = do
  writeSession emptySession
  redirect "/"

authRoutes :: OAuth2 -> App ()
authRoutes oauth2 = do
  -- Auth
  get ("github" <//> "login")    (startGithubAuthentication oauth2)
  get ("github" <//> "callback") (authorizeAndLogin oauth2)
  get ("github" <//> "logout")   clearSession
