{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Semigroup ((<>))
import Options.Applicative

import Lodjur.Deploy
import Lodjur.Web

main :: IO ()
main =
  startServices =<< execParser opts
  where
    opts = info (lodjur <**> helper)
      ( fullDesc
     <> progDesc "Lodjur"
     <> header "Mpowered's Nixops Deployment Frontend" )

    startServices Options{..} = do
      env <- newLodjurEnv nixopsDeployment gitWorkingDir
      runServer port env

data Options = Options
  { gitWorkingDir    :: FilePath
  , nixopsDeployment :: Deployment
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
