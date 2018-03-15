{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Main where

import qualified Data.HashSet           as HashSet
import           Data.Semigroup         ((<>))
import           Database.SQLite.Simple
import           Options.Applicative

import qualified Lodjur.Deployer        as Deployer
import           Lodjur.Deployment
import qualified Lodjur.EventLogger     as EventLogger
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
    conn        <- open databaseName
    eventLogger <- spawn =<< EventLogger.initialize conn
    deployer    <- spawn
      =<< Deployer.initialize eventLogger deploymentNames gitWorkingDir conn
    runServer port deployer eventLogger

data Options = Options
  { gitWorkingDir     :: FilePath
  , nixopsDeployments :: [DeploymentName]
  , port              :: Port
  , databaseName      :: FilePath
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
          (  long "database"
          <> metavar "FILE"
          <> help "Path to database"
          <> showDefault
          <> value ":memory:"
          )
