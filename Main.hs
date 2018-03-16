{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Main where

import qualified Data.HashSet               as HashSet
import           Data.Semigroup             ((<>))
import           Database.PostgreSQL.Simple
import           Options.Applicative

import qualified Lodjur.Deployer            as Deployer
import           Lodjur.Deployment
import qualified Lodjur.EventLogger         as EventLogger
import qualified Lodjur.OutputLogger        as OutputLogger
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

  startServices Options {..} = do
    let deploymentNames = HashSet.fromList nixopsDeployments
    pool <- newPool ConnectInfo
      { connectHost     = databaseHost
      , connectPort     = databasePort
      , connectName     = databaseName
      , connectUser     = databaseUser
      , connectPassword = databasePassword
      }
    eventLogger  <- spawn =<< EventLogger.initialize pool
    outputLogger <- spawn =<< OutputLogger.initialize pool
    deployer     <-
      spawn
        =<< Deployer.initialize eventLogger
                                outputLogger
                                deploymentNames
                                gitWorkingDir
                                conn
    runServer port deployer eventLogger

data Options = Options
  { gitWorkingDir     :: FilePath
  , nixopsDeployments :: [DeploymentName]
  , port              :: Port
  , databaseHost      :: String
  , databasePort      :: Word16
  , databaseName      :: String
  , databaseUser      :: String
  , databasePassword  :: String
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
          (  long "database-host"
          <> metavar "HOST"
          <> help "PostgreSQL host"
          <> showDefault
          <> value "localhost"
          )
    <*> option
          auto
          (  long "database-port"
          <> metavar "PORT"
          <> help "PostgreSQL port"
          <> showDefault
          <> value 5432
          )
    <*> strOption
          (  long "database-name"
          <> metavar "DATABASE"
          <> help "Name of PostgreSQL database"
          <> showDefault
          <> value "lodjur"
          )
    <*> strOption
          (  long "database-user"
          <> metavar "USER"
          <> help "PostgreSQL user name"
          <> showDefault
          <> value "root"
          )
    <*> strOption
          (  long "database-password"
          <> metavar "PASSWORD"
          <> help "PostgreSQL user password"
          <> value ""
          )
