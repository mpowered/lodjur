{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Main where

import qualified Data.HashSet        as HashSet
import           Data.Semigroup      ((<>))
import           Options.Applicative

import           Lodjur.Deploy
import           Lodjur.Process
import           Lodjur.Web

main :: IO ()
main =
  startServices =<< execParser opts
  where
    opts = info (lodjur <**> helper)
      ( fullDesc
     <> progDesc "Lodjur"
     <> header "Mpowered's Nixops Deployment Frontend" )

    startServices Options{..} = do
      let deploymentNames = HashSet.fromList nixopsDeployments
      eventLogger <- spawn (EventLogger mempty)
      deployer <- spawn (initialize eventLogger deploymentNames gitWorkingDir)
      runServer port deployer eventLogger

data Options = Options
  { gitWorkingDir     :: FilePath
  , nixopsDeployments :: [DeploymentName]
  , port              :: Port
  }

lodjur :: Parser Options
lodjur = Options
      <$> strOption
          ( long "git-working-dir"
         <> metavar "PATH"
         <> short 'g'
         <> help "Path to Git directory containing deployment expressions" )
      <*> many (strOption
          ( long "deployment"
         <> metavar "NAME"
         <> short 'd'
         <> help "Names of nixops deployments to support" ))
      <*> option auto
          ( long "port"
         <> metavar "PORT"
         <> short 'p'
         <> help "Port to run the web server on"
         <> showDefault
         <> value 4000 )
