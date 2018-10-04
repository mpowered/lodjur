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

exchangeCode :: OAuth2 -> Action ()
exchangeCode oauth2 = do
  merr   <- param "error"
  case merr of
    Just err -> do
      setStatus status400
      allParams <- params
      text ("Error: " <> err <> ", Params: " <> Text.pack (show allParams))
    Nothing -> do
      code   <- param' "code"
      result <- liftIO $ do
        manager <- newManager tlsManagerSettings
        fetchAccessToken manager oauth2 (ExchangeToken code)
      case result of
        Left err -> do
          setStatus status400
          writeSession Session { currentUser = Nothing, continueTo = Nothing }
          text (Text.pack (show err))
        Right OAuth2Token {..} -> do
          githubResult <- liftIO . runExceptT $ do
            user <- ExceptT (getUser accessToken)
            teams <- ExceptT (getTeams accessToken)
            return (user, teams)
          case githubResult of
            Left err -> do
              setStatus status400
              text ("Failed: " <> Text.pack (show err))
            Right (githubUser, teams)
              | isAuthorized teams -> do
                let user = User { userId = UserId (GitHub.untagName $ GitHub.userLogin githubUser) }
                continueTo' <- continueTo <$> readSession
                writeSession Session { currentUser = Just user, continueTo = Nothing }
                case continueTo' of
                  Just relRef -> redirect (Text.decodeUtf8 (serializeURIRef' relRef))
                  Nothing -> redirect "/"
              | otherwise -> do
                setStatus status403
                text "You are not authorized, because you don't have read/write permissions in the mpowered/core team."

isAuthorized :: [GitHub.Team] -> Bool
isAuthorized = any authorizedInTeam
  where
    authorizedInTeam t = team t == "core" && org t == "mpowered"
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

authRoutes :: OAuth2 -> App ()
authRoutes oauth2 = do
  -- Auth
  get ("auth" <//> "github" <//> "login")    (startGithubAuthentication oauth2)
  get ("auth" <//> "github" <//> "callback") (exchangeCode oauth2)
  get ("auth" <//> "github" <//> "logout")   clearSession
