{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Main where

import           Data.ByteString             (ByteString)
import qualified Data.HashSet                as HashSet
import           Data.Semigroup              ((<>))
import           Data.Text                  (Text)
import           Data.Word
import           Database.PostgreSQL.Simple
import           Options.Applicative

import qualified Lodjur.Database             as Database
import           Lodjur.Deployment
import qualified Lodjur.Deployment.Deployer  as Deployer
import qualified Lodjur.Events.EventLogger   as EventLogger
import qualified Lodjur.Git.GitAgent         as GitAgent
import qualified Lodjur.Git.GitReader        as GitReader
import qualified Lodjur.Output.OutputLoggers as OutputLoggers
import           Lodjur.Process
import           Lodjur.Web

main :: IO ()
main = startServices =<< execParser opts
 where
  opts = info
    (lodjur <**> helper)
    ( fullDesc <> progDesc "Lodjur" <> header
      "Mpowered's Nixops Deployment Frontend"
    )

  stripes        = 4
  ttl            = 5
  connsPerStripe = 4

  startServices Options {..} = do
    let deploymentNames = HashSet.fromList nixopsDeployments
    pool <- Database.newPool
      ConnectInfo
        { connectHost     = databaseHost
        , connectPort     = databasePort
        , connectDatabase = databaseName
        , connectUser     = databaseUser
        , connectPassword = databasePassword
        }
      stripes
      ttl
      connsPerStripe

    eventLogger   <- spawn =<< EventLogger.initialize pool
    outputLoggers <- spawn =<< OutputLoggers.initialize pool
    gitAgent      <- spawn =<< GitAgent.initialize gitWorkingDir
    gitReader     <- spawn =<< GitReader.initialize gitWorkingDir
    deployer      <-
      spawn
        =<< Deployer.initialize eventLogger
                                outputLoggers
                                gitAgent
                                deploymentNames
                                pool
    runServer port
              (authUser, authPassword)
              deployer
              eventLogger
              outputLoggers
              gitAgent
              gitReader
              githubSecretToken
              githubRepos

data Options = Options
  { gitWorkingDir     :: FilePath
  , nixopsDeployments :: [DeploymentName]
  , port              :: Port
  , authUser          :: ByteString
  , authPassword      :: ByteString
  , databaseHost      :: String
  , databasePort      :: Word16
  , databaseName      :: String
  , databaseUser      :: String
  , databasePassword  :: String
  , githubSecretToken :: ByteString
  , githubRepos       :: [Text]
  , staticDirectory   :: FilePath
  }

lodjur :: Parser Options
lodjur =
  Options
    <$> strOption
          ( long "git-working-dir" <> metavar "PATH" <> short 'g' <> help
            "Path to Git directory containing deployment expressions"
          )
    <*> many
          ( strOption
            ( long "deployment" <> metavar "NAME" <> short 'd' <> help
              "Names of nixops deployments to support"
            )
          )
    <*> option
          auto
          (  long "port"
          <> metavar "PORT"
          <> short 'p'
          <> help "Port to run the web server on"
          <> showDefault
          <> value 4000
          )
    <*> strOption
          (  long "auth-user"
          <> metavar "USERNAME"
          <> help "Basic authentication user name"
          <> showDefault
          <> value ""
          )
    <*> strOption
          (  long "auth-password"
          <> metavar "PASSWORD"
          <> help "Basic authentication password"
          <> showDefault
          <> value ""
          )
    <*> strOption
          (  long "database-host"
          <> metavar "HOST"
          <> short 'H'
          <> help "PostgreSQL host"
          <> showDefault
          <> value "localhost"
          )
    <*> option
          auto
          (  long "database-port"
          <> metavar "PORT"
          <> short 'P'
          <> help "PostgreSQL port"
          <> showDefault
          <> value 5432
          )
    <*> strOption
          (  long "database-name"
          <> metavar "DATABASE"
          <> short 'D'
          <> help "Name of PostgreSQL database"
          <> showDefault
          <> value "lodjur"
          )
    <*> strOption
          (  long "database-user"
          <> metavar "USER"
          <> short 'U'
          <> help "PostgreSQL user name"
          <> showDefault
          <> value "root"
          )
    <*> strOption
          (  long "database-password"
          <> metavar "PASSWORD"
          <> short 'W'
          <> help "PostgreSQL user password"
          <> value ""
          )
    <*> strOption
          (  long "github-secret"
          <> metavar "TOKEN"
          <> short 's'
          <> help "Shared secret for Github webhooks to validate signatures"
          <> value ""
          )
    <*> many
          ( strOption
            (  long "github-repo"
            <> metavar "FULL-REPO-NAME"
            <> short 'r'
            <> help "Github repository name to allow webhooks for"
            )
          )
    <*> strOption
          (  long "static-directory"
          <> metavar "DIRECTORY"
          <> help "Path to directory of Lodjur static assets"
          <> value "static"
          )
