{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.HashSet               as HashSet
import           Data.Semigroup ((<>))
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
      let deploymentNames = HashSet.singleton nixopsDeployment
      deployer <- spawn (initialize deploymentNames gitWorkingDir)
      -- env <- newLodjurEnv nixopsDeployment gitWorkingDir
      runServer port deployer

data Options = Options
  { gitWorkingDir    :: FilePath
  , nixopsDeployment :: DeploymentName
  , port             :: Port
  }

lodjur :: Parser Options
lodjur = Options
      <$> strOption
          ( long "git-working-dir"
         <> metavar "PATH"
         <> short 'g'
         <> help "Path to Git directory containing deployment expressions" )
      <*> strOption
          ( long "deployment"
         <> short 'd'
         <> help "Name of nixops deployment" )
      <*> option auto
          ( long "port"
         <> help "Port to run the web server on"
         <> showDefault
         <> value 4000
         <> metavar "PORT" )
