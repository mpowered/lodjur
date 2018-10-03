{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE RecordWildCards   #-}

module Main where

import           Data.Aeson                   as JSON
import           Data.ByteString              (ByteString)
import           Data.ByteString.Char8        as Char8
import           Data.Semigroup               ((<>))
import           Data.Text                    (Text)
import           Data.Text.IO                 as Text
import           Data.Text.Encoding           as Text
import           Database.PostgreSQL.Simple
import           Network.OAuth.OAuth2
import           Options.Applicative
import           Text.Toml
import           URI.ByteString hiding (Port)
import           URI.ByteString.QQ

import qualified Lodjur.Database              as Database
import           Lodjur.Deployment
import qualified Lodjur.Deployment.Deployer   as Deployer
import qualified Lodjur.Events.EventLogger    as EventLogger
import qualified Lodjur.Git.GitAgent          as GitAgent
import qualified Lodjur.Git.GitReader         as GitReader
import qualified Lodjur.Output.OutputLoggers  as OutputLoggers
import qualified Lodjur.Output.OutputStreamer as OutputStreamer
import           Lodjur.Process
import           Lodjur.Web

data DeploymentConfiguration = DeploymentConfiguration
  { name :: Text
  , warn :: Bool
  }

deploymentConfigurationToDeployment :: DeploymentConfiguration -> Deployment
deploymentConfigurationToDeployment DeploymentConfiguration {..} =
  Deployment {deploymentName = DeploymentName name, deploymentWarn = warn}

data LodjurConfiguration = LodjurConfiguration
  { gitWorkingDir       :: FilePath
  , deployments         :: [DeploymentConfiguration]
  , port                :: Port
  , databaseConnectInfo ::  ConnectInfo
  , githubSecretToken   :: ByteString
  , githubRepos         :: [Text]
  , githubOauth         :: OAuth2
  , staticDirectory     :: FilePath
  }

instance FromJSON DeploymentConfiguration where
  parseJSON = withObject "Deployment" $ \o -> do
    name <- o .: "name"
    warn <- o .: "warn"
    return DeploymentConfiguration{..}

instance FromJSON LodjurConfiguration where
  parseJSON = withObject "Configuration" $ \o -> do
    gitWorkingDir <- o .: "git-working-dir"
    deployments <- o .: "deployments"
    port <- o .: "http-port"
    db <- o .: "database"
    databaseConnectInfo <- parseDatabaseConnectInfo db
    githubSecretToken <- Char8.pack <$> (o .: "github-secret-token")
    githubRepos <- o .: "github-repos"
    staticDirectory <- o .: "static-directory"

    oauthClientId <- o .: "github-oauth-client-id"
    oauthClientSecret <- o .: "github-oauth-client-secret"
    oauthCallbackUrlStr <- o .: "github-oauth-callback-url"
    oauthCallback <- either (fail . show) (pure . pure) (parseURI strictURIParserOptions (Text.encodeUtf8 oauthCallbackUrlStr))
    let githubOauth = OAuth2
          { oauthOAuthorizeEndpoint = [uri|https://github.com/login/oauth/authorize|]
          , oauthAccessTokenEndpoint = [uri|https://github.com/login/oauth/access_token|]
          , ..
          }

    return LodjurConfiguration{..}
    where
      parseDatabaseConnectInfo o = do
        databaseHost <- o .: "host"
        databasePort <- o .: "port"
        databaseName <- o .: "name"
        databaseUser <- o .: "user"
        databasePassword <- o .: "password"
        return ConnectInfo
          { connectHost     = databaseHost
          , connectPort     = databasePort
          , connectDatabase = databaseName
          , connectUser     = databaseUser
          , connectPassword = databasePassword
          }


newtype LodjurOptions = LodjurOptions
  { configFile :: FilePath
  }

lodjur :: Parser LodjurOptions
lodjur = LodjurOptions <$> strOption
  (  long "config-file"
  <> metavar "PATH"
  <> short 'c'
  <> value "lodjur.toml"
  <> help "Path to Lodjur configuration file"
  )

readConfiguration :: FilePath -> IO LodjurConfiguration
readConfiguration path = do
  f <- Text.readFile path
  case parseTomlDoc path f of
    Right toml -> case fromJSON (toJSON toml) of
      JSON.Success config -> pure config
      JSON.Error   e      -> fail e
    Left e -> fail (show e)


main :: IO ()
main = startServices =<< execParser opts
 where
  opts = info
    (lodjur <**> helper)
    (fullDesc <> progDesc "Lodjur" <> header
      "Mpowered's Nixops Deployment Frontend"
    )

  stripes        = 4
  ttl            = 5
  connsPerStripe = 4

  startServices LodjurOptions {..} = do
    LodjurConfiguration {..} <- readConfiguration configFile
    pool <- Database.newPool databaseConnectInfo stripes ttl connsPerStripe

    eventLogger              <- spawn =<< EventLogger.initialize pool
    outputLoggers            <- spawn =<< OutputLoggers.initialize pool
    outputStreamer           <- spawn =<< OutputStreamer.initialize pool
    gitAgent                 <- spawn =<< GitAgent.initialize gitWorkingDir
    gitReader                <- spawn =<< GitReader.initialize gitWorkingDir
    deployer                 <-
      spawn
        =<< Deployer.initialize
              eventLogger
              outputLoggers
              gitAgent
              (deploymentConfigurationToDeployment <$> deployments)
              pool

    -- Fetch on startup in case we miss webhooks while service is not running
    gitAgent ! GitAgent.FetchRemote

    runServer port
              staticDirectory
              deployer
              eventLogger
              outputLoggers
              outputStreamer
              gitAgent
              gitReader
              githubSecretToken
              githubRepos
              githubOauth
