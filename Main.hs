{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Main where

import           Control.Concurrent
import           Control.Concurrent.Async
import           Data.Semigroup               ((<>))
import           Data.Text                    (Text)
import qualified Data.Text                    as Text
import qualified Data.Text.Encoding           as Text
import           Data.Time.Clock              (UTCTime, getCurrentTime)
import           Data.Time.Clock.POSIX
import qualified Database.Redis               as Redis
import           Network.HTTP.Client
import           Network.HTTP.Client.TLS
import           Options.Applicative

import           Config
import qualified Lodjur.Database              as Database
import qualified Lodjur.GitHub                as GH
import qualified Lodjur.Jobs                  as Jobs
import qualified Lodjur.JobQueue              as Jobs
import           Lodjur.Web
import           Lodjur.Web.Base
import           Lodjur.Web.Messenger

import           GitHub
import           GitHub.Data.Id
import           GitHub.Extra
import           GitHub.Endpoints.Apps        hiding (app)
import           GitHub.Endpoints.Checks
import qualified Web.JWT                      as JWT

newtype LodjurOptions = LodjurOptions
  { configFile          :: FilePath
  }

lodjur :: Parser LodjurOptions
lodjur = LodjurOptions
  <$> strOption
    (  long "config-file"
    <> metavar "PATH"
    <> short 'c'
    <> value "lodjur.toml"
    <> help "Path to Lodjur configuration file"
    )

main :: IO ()
main = start =<< execParser opts
 where
  opts = info
    (lodjur <**> helper)
    (fullDesc <> progDesc "Lodjur" <> header
      "Mpowered's Nixops Deployment Frontend"
    )

start :: LodjurOptions -> IO ()
start LodjurOptions {..} = do
  Config
    { githubRepos = envGithubRepos
    , githubSecretToken = envGithubSecretToken
    , githubAppId = envGithubAppId
    , githubAppSigner = envGithubAppSigner
    , githubInstallationId = envGithubInstallationId
    , ..
    } <- readConfiguration configFile

  envManager <- newManager tlsManagerSettings

  let stripes        = 4
      ttl            = 5
      connsPerStripe = 4

  envDbPool <- Database.newPool databaseConnectInfo stripes ttl connsPerStripe
  envRedisConn <- Redis.checkedConnect redisConnectInfo

  envGithubInstallationAccessToken <- newMVar Nothing

  let env = Env{..}

  -- subscribeStatuses envRedisConn (statusWatcher env)

  githubToken <-
    GH.installationToken
      envManager
      (Id envGithubAppId)
      envGithubAppSigner
      (Id envGithubInstallationId)

  withAsync (messenger envRedisConn githubToken) $ \ma -> do
    runServer httpPort staticDirectory env githubOauth githubTeamAuth
    wait ma

{-
statusWatcher :: Env -> Jobs.StatusChanged -> IO Bool
statusWatcher env@Env{..} Jobs.StatusChanged{..} = do
  l <- Jobs.lookup envRedisConn changedJobId
  case l of
    Nothing -> return True
    Just (Left _) -> return True
    Just (Right job) -> notifyGithub job
  where
    notifyGithub Jobs.CheckJob{..} = do
      auth <- getInstallationAccessToken env
      r <- executeRequestWithMgr envManager auth $
        updateCheckRunR
          (Name $ Jobs.repoOwner checkRepo)
          (Name $ Jobs.repoName checkRepo)
          (Id $ undefined) -- check run id
          case changedStatus of
            Jobs.Queued ->
              UpdateCheckRun
                { updateCheckRunStatus       = Just "queued"
                , updateCheckRunConclusion   = Nothing
                }
            Jobs.InProgress ->
              UpdateCheckRun
                { updateCheckRunStatus       = Just "in_progress"
                , updateCheckRunConclusion   = Nothing
                }
            Jobs.Completed conclusion ->
              UpdateCheckRun
                { updateCheckRunStatus       = Just "completed"
                , updateCheckRunConclusion   =
                    case conclusion of
                      Jobs.Cancelled -> "cancelled"
                      Jobs.TimedOut -> "timed_out"
                      Jobs.Failed -> "failed"
                      Jobs.Neutral -> "neutral"
                      Jobs.Success -> "success"
                }
      print r

createInstallationJWT :: Env -> IO Token
createInstallationJWT Env{..} = do
  now <- getPOSIXTime
  let claims = mempty { JWT.iss = JWT.stringOrURI (Text.pack $ show envGithubAppId)
                      , JWT.iat = JWT.numericDate now
                      , JWT.exp = JWT.numericDate (now + 600)
                      }
      jwt = JWT.encodeSigned envGithubAppSigner claims
  return $ Text.encodeUtf8 jwt

newInstallationAccessToken :: Env -> IO AccessToken
newInstallationAccessToken env@Env{..} = do
  tok <- createInstallationJWT env
  result <- executeRequestWithMgr envManager (Bearer tok) $
              createInstallationTokenR (Id envGithubInstallationId)
  case result of
    Left err ->
      error $ "Unable to fetch installation access token: " <> Text.pack (show err)
    Right token ->
      return token

getInstallationAccessToken :: Env -> IO Auth
getInstallationAccessToken env@Env{..} = do
  now <- getCurrentTime
  let renew = do
        at <- newInstallationAccessToken env
        putMVar envGithubInstallationAccessToken (Just at)
        return (asAuth at)
  tok <- takeMVar envGithubInstallationAccessToken
  case tok of
    Just at ->
      case accessTokenExpiresAt at of
        Just e ->
          if now >= e then renew else return (asAuth at)
        Nothing ->
          return (asAuth at)
    Nothing -> renew
  where
    asAuth = OAuth . accessToken
-}
