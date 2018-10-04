{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Lodjur.Web.Auth.GitHub where

import           Control.Monad.Except
import           Control.Monad.IO.Class               (liftIO)
import           Data.Foldable                        (toList)
import qualified Data.Text                            as Text
import qualified Data.Text.Encoding                   as Text
import qualified GitHub
import qualified GitHub.Endpoints.Organizations.Teams as GitHub
import qualified GitHub.Endpoints.Users               as GitHub
import           Network.HTTP.Client
import           Network.HTTP.Client.TLS
import           Network.HTTP.Types.Status
import           Network.OAuth.OAuth2
import           URI.ByteString
import           Web.Spock

import           Lodjur.Auth
import           Lodjur.User
import           Lodjur.Web.Base

startGithubAuthentication :: OAuth2 -> Action ()
startGithubAuthentication oauth2 = do
  let
    scope   = "read:org"
    state   = "foobar"
    params' = [("scope", scope), ("state", state), ("allow_signup", "false")]
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

exchangeCode :: OAuth2 -> Action AccessToken
exchangeCode oauth2 = do
  code   <- param' "code"
  result <- liftIO $ do
    manager <- newManager tlsManagerSettings
    fetchAccessToken manager oauth2 (ExchangeToken code)
  case result of
    Left err -> do
      setStatus status400
      writeSession Session { currentUser = Nothing, continueTo = Nothing }
      text (Text.pack (show err))
    Right OAuth2Token {..} ->
      return accessToken

getUserAndTeams :: AccessToken -> Action (GitHub.User, [GitHub.Team])
getUserAndTeams accessToken = do
  githubResult <- liftIO . runExceptT $ do
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
  writeSession Session { currentUser = Just user, continueTo = Nothing }
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

getUser :: AccessToken -> IO (Either GitHub.Error GitHub.User)
getUser at =
  -- TODO use manager?
  GitHub.userInfoCurrent' (toOAuth at)

getTeams :: AccessToken -> IO (Either GitHub.Error [GitHub.Team])
getTeams at =
  -- TODO use manager?
  fmap toList <$> GitHub.listTeamsCurrent' (toOAuth at)

clearSession :: Action ()
clearSession = do
  writeSession Session { currentUser = Nothing, continueTo = Nothing }
  redirect "/"

authRoutes :: OAuth2 -> TeamAuthConfig -> App ()
authRoutes oauth2 teamAuthCfg = do
  -- Auth
  get ("auth" <//> "github" <//> "login")    (startGithubAuthentication oauth2)
  get ("auth" <//> "github" <//> "callback") (authorizeAndLogin oauth2 teamAuthCfg)
  get ("auth" <//> "github" <//> "logout")   clearSession
