{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Main where

import           Control.Concurrent
import           Control.Concurrent.Async
import           Data.Semigroup               ((<>))
import qualified Database.Redis               as Redis
import           Network.HTTP.Client
import           Network.HTTP.Client.TLS
import           Options.Applicative

import           Config
-- import qualified Lodjur.Database              as Database
import qualified Lodjur.GitHub                as GH
import           Web
import           Web.Base
import           Web.Messenger

import           GitHub.Data.Id

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

  -- let stripes        = 4
  --     ttl            = 5
  --     connsPerStripe = 4
  -- envDbPool <- Database.newPool databaseConnectInfo stripes ttl connsPerStripe
  envRedisConn <- Redis.checkedConnect redisConnectInfo

  envGithubInstallationAccessToken <- newMVar Nothing

  let env = Env{..}

  githubToken <-
    GH.installationToken
      envManager
      (Id envGithubAppId)
      envGithubAppSigner
      (Id envGithubInstallationId)

  withAsync (messenger envRedisConn githubToken) $ \ma -> do
    runServer httpPort env githubOauth
    wait ma
