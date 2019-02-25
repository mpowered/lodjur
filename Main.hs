{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Main where

import           Control.Concurrent
import           Data.Semigroup               ((<>))
import qualified Data.Text                    as Text
import qualified Data.Text.Encoding           as Text
import           Data.Time.Clock.POSIX
import           GitHub
import           GitHub.Data.Id               (Id(..))
import           GitHub.Extra
import           GitHub.Endpoints.Apps
import           Network.HTTP.Client
import           Network.HTTP.Client.TLS
import           Options.Applicative
import qualified Web.JWT                      as JWT

import           Config
-- import           Lodjur.Auth
import qualified Lodjur.Database              as Database
-- import           Lodjur.Deployment
-- import qualified Lodjur.Deployment.Deployer   as Deployer
-- import qualified Lodjur.Events.EventLogger    as EventLogger
-- import qualified Lodjur.Git.GitAgent          as GitAgent
-- import qualified Lodjur.Git.GitReader         as GitReader
-- import qualified Lodjur.Output.OutputLoggers  as OutputLoggers
-- import qualified Lodjur.Output.OutputStreamer as OutputStreamer
-- import           Lodjur.Process
import           Lodjur.Web
import           Lodjur.Web.Base

data LodjurOptions = LodjurOptions
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

    -- envEventLogger              <- spawn =<< EventLogger.initialize pool
    -- envOutputLoggers            <- spawn =<< OutputLoggers.initialize pool
    -- envOutputStreamer           <- spawn =<< OutputStreamer.initialize pool
    -- envGitAgent                 <- spawn =<< GitAgent.initialize gitWorkingDir
    -- envGitReader                <- spawn =<< GitReader.initialize gitWorkingDir
    -- envDeployer                 <-
    --   spawn
    --     =<< Deployer.initialize
    --           envEventLogger
    --           envOutputLoggers
    --           envGitAgent
    --           gitWorkingDir
    --           (deploymentConfigurationToDeployment <$> deployments)
    --           pool
  installationAccessToken <- newMVar Nothing

  let env = Env { envGithubInstallationAccessToken = installationAccessToken
                , ..
                }

    -- Fetch on startup in case we miss webhooks while service is not running
    -- envGitAgent ! GitAgent.FetchRemote

  runServer httpPort staticDirectory env githubOauth githubTeamAuth
