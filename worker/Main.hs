{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ExtendedDefaultRules  #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TypeFamilies          #-}

module Main where

import           Control.Concurrent
import           Control.Monad
import           Control.Monad.Catch          (MonadMask, bracket)
import           Control.Monad.Except
import           Control.Monad.Log
import           Control.Monad.Reader
import           Data.Text                    (Text)
import qualified Data.Text                    as Text
import           Data.Text.Prettyprint.Doc
import           Network.HTTP.Client          (newManager, defaultManagerSettings)
import           Options.Applicative          hiding (Success, Failure)
import           System.Directory
import           Prelude                      hiding (lookup)

import           Network.Socket               (withSocketsDo)

import qualified Lodjur.GitHub                as GH
import           Lodjur.Logging
import           Lodjur.Core.Websocket        as WS
import qualified Lodjur.Job                   as Job

import           Config
import           Env
import qualified Build
import qualified Git
import qualified RSpec
import           Types

default (Text)

newtype Options = Options
  { configFile :: FilePath
  }

lodjur :: Parser Options
lodjur = Options
  <$> strOption
    (  long "config-file"
    <> metavar "PATH"
    <> short 'c'
    <> value "lodjur-worker.dhall"
    <> help "Path to Lodjur Worker configuration file"
    )

main :: IO ()
main = start =<< execParser opts
 where
  opts = info
    (lodjur <**> helper)
    (fullDesc <> progDesc "Lodjur Worker" <> header
      "Performs check jobs submitted by Lodjur"
    )

start :: Options -> IO ()
start Options{..} = do
  httpManager <- newManager defaultManagerSettings
  cfg <- readConfig configFile
  env <- ensureEnv httpManager cfg
  withSocketsDo $
    WS.runClient (envWebSocket env) (envLogTarget env) (\r c -> runWorker env (handler r c))
  where
    ensureEnv httpManager cfg = do
      e <- runExceptT $ buildEnv httpManager cfg
      case e of
        Left err -> fail err
        Right a -> return a


handler :: Job.Request -> Chan Job.Reply -> Worker Job.Result
handler (Job.Request _name src act) chan = do
  env@Env{..} <- ask
  case act of
    Job.Build doCheck -> do
      res <- withWorkDir env src $ \workdir ->
        Build.build chan workdir ".lodjur/build.nix"
      if doCheck && (Job.conclusion res == Job.Success)
       then do
        let checkdeps =
              [ Job.Request "check-toolkit1" src (Job.Check "toolkit1")
              , Job.Request "check-toolkit2" src (Job.Check "toolkit2")
              , Job.Request "check-toolkit3" src (Job.Check "toolkit3")
              , Job.Request "check-sms"      src (Job.Check "sms")
              , Job.Request "check-beagle"   src (Job.Check "beagle")
              ]
        return $ res
          { Job.conclusion   = Job.Neutral
          , Job.dependencies = Job.dependencies res ++ checkdeps
          }
       else
        return res

    Job.Check app ->
      withWorkDir env src $ \workdir -> do
        res <- Build.build chan workdir ".lodjur/build.nix"
        case Job.conclusion res of
          Job.Success -> RSpec.rspec chan workdir (Text.unpack app)
          _ -> return res

    _ ->
      return $ Job.Result Job.Failure Nothing [] Nothing

withWorkDir
  :: (MonadIO io, MonadMask io, MonadLog LogMsg io)
  => Env
  -> GH.GitHubCommit
  -> (FilePath -> io a)
  -> io a
withWorkDir Env{..} commit
  = bracket
    (Git.checkout envGit envGithub commit)
    (liftIO . removeDirectoryRecursive)
