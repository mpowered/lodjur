{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Lodjur.Web.Auth.GitHub where

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
import           Web.Spock (HasSpock (..))
import           Web.Spock.Core
import           Web.Spock.SessionActions

import           Lodjur.Auth
import           Lodjur.User
import           Lodjur.Web.Base

startGithubAuthentication :: OAuth2 -> Action ()
startGithubAuthentication oauth2 = do
  state <- liftIO (UUID.toText <$> UUID.nextRandom)
  modifySession (\s -> s { oauthState = Just state })
  let
    scope   = "read:org"
    params' = [("scope", scope), ("state", Text.encodeUtf8 state), ("allow_signup", "false")]
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
    fetchAccessToken envManager oauth2 (ExchangeToken code)
  case result of
    Left err -> do
      setStatus status400
      writeSession emptySession
      text (Text.pack (show err))
    Right OAuth2Token {..} ->
      return accessToken

getUserAndTeams :: AccessToken -> Action (GitHub.User, [GitHub.Team])
getUserAndTeams accessToken = do
  githubResult <- runExceptT $ do
    user <- ExceptT (getUser accessToken)
    teams <- ExceptT (getTeams accessToken)
    return (user, teams)
  case githubResult of
    Left err -> do
      setStatus status400
      text ("Failed: " <> Text.pack (show err))
    Right userAndTeams ->
      return userAndTeams

loginAndRedirect :: GitHub.User -> Action ()
loginAndRedirect githubUser = do
  let user = User { userId = UserId (GitHub.untagName $ GitHub.userLogin githubUser) }
  continueTo' <- continueTo <$> readSession
  writeSession emptySession { currentUser = Just user }
  case continueTo' of
    Just relRef -> redirect (Text.decodeUtf8 (serializeURIRef' relRef))
    Nothing -> redirect "/"

authorizeAndLogin :: OAuth2 -> TeamAuthConfig -> Action ()
authorizeAndLogin oauth2 teamAuth = do
  checkError
  accessToken <- exchangeCode oauth2
  (githubUser, teams) <- getUserAndTeams accessToken
  if isAuthorized teamAuth teams
    then loginAndRedirect githubUser
    else do setStatus status403
            text (mconcat [ "You are not authorized, "
                          , "because you don't have read/write permissions in the "
                          , githubAuthOrg teamAuth <> "/" <> githubAuthTeam teamAuth
                          , " team."
                          ])

isAuthorized :: TeamAuthConfig -> [GitHub.Team] -> Bool
isAuthorized TeamAuthConfig{..} = any authorizedInTeam
  where
    authorizedInTeam t = team t == githubAuthTeam && org t == githubAuthOrg
    team = GitHub.untagName . GitHub.teamSlug
    org  = GitHub.untagName . GitHub.simpleOrganizationLogin . GitHub.teamOrganization

toOAuth :: AccessToken -> GitHub.Auth
toOAuth (AccessToken accessToken) =
  GitHub.OAuth (Text.encodeUtf8 accessToken)

getUser :: AccessToken -> Action (Either GitHub.Error GitHub.User)
getUser at = do
  Env {..} <- getState
  liftIO $
    GitHub.executeRequestWithMgr
      envManager
      (toOAuth at)
      GitHub.userInfoCurrentR

getTeams :: AccessToken -> Action (Either GitHub.Error [GitHub.Team])
getTeams at = do
  Env {..} <- getState
  liftIO . fmap (fmap toList) $
    GitHub.executeRequestWithMgr
      envManager
      (toOAuth at)
      (GitHub.listTeamsCurrentR GitHub.FetchAll)

clearSession :: Action ()
clearSession = do
  writeSession emptySession
  redirect "/"

authRoutes :: OAuth2 -> TeamAuthConfig -> App ()
authRoutes oauth2 teamAuthCfg = do
  -- Auth
  get ("auth" <//> "github" <//> "login")    (startGithubAuthentication oauth2)
  get ("auth" <//> "github" <//> "callback") (authorizeAndLogin oauth2 teamAuthCfg)
  get ("auth" <//> "github" <//> "logout")   clearSession
