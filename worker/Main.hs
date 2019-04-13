{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ExtendedDefaultRules  #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TypeFamilies          #-}

module Main where

import           Control.Concurrent
import           Control.Concurrent.Async
import           Control.Monad
import           Control.Error
import           Control.Monad.Catch          (MonadMask, bracket, onException)
import           Control.Monad.IO.Class
import           Control.Monad.Log
import           Control.Monad.Reader
import           Data.Aeson                   hiding (Options, Success)
import           Data.Aeson.Encode.Pretty     (encodePretty)
import           Data.ByteString              (ByteString)
import qualified Data.ByteString.Lazy         as ByteString
import           Data.Text                    (Text)
import qualified Data.Text                    as Text
import qualified Data.Text.Encoding           as Text
import           Data.Text.Prettyprint.Doc
import qualified Data.UUID                    as UUID
import qualified Data.UUID.V4                 as UUID
import qualified Data.Vector                  as Vec
import           GHC.Generics
import           Options.Applicative          hiding (Success, Failure)
import           System.Directory
import           Prelude                      hiding (lookup)

import           Network.Socket               (withSocketsDo)
import           Network.WebSockets

import qualified Lodjur.GitHub                as GH
import           Lodjur.Logging
import           Lodjur.Core.Websocket        as WS
import qualified Lodjur.Job                   as Job

import           Config
import           Env
import qualified Build
import qualified Git
import qualified RSpec
import qualified RSpec.Results                as RSpec
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
    <> value "lodjur-worker.toml"
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
  Config{..} <- readConfiguration configFile
  gitEnv <- Git.setupEnv gitCfg
  -- let logTarget = maybe LogStdout LogFile logFile
  withSocketsDo $ WS.runClient managerCI (runWorker Env{..} . handler)

handler :: Job.Request -> Worker Job.Result
handler (Job.Request _name src act) =
  case act of
    Job.Build -> build src
    Job.Check app -> check src app
    _ -> do
      -- logError $ "Unsupported message:" <+> nest 4 (viaShow unsupported)
      return (Job.Result Job.Failure [] [])

withWorkDir
  :: (MonadIO io, MonadMask io)
  => Env
  -> GH.RepoRef
  -> GH.Sha
  -> (FilePath -> io a)
  -> io a
withWorkDir Env {..} repo sha
  = bracket
    (liftIO $ Git.checkout gitEnv repo sha)
    (liftIO . removeDirectoryRecursive)

build :: GH.Source -> Worker Job.Result
build src@GH.Source{..} = do
  env@Env{..} <- ask
  -- logInfo "Build started"
  rep <- withWorkDir env (GH.RepoRef owner repo) sha $ \workdir ->
    Build.build workdir ".lodjur/build.nix" src
  -- logInfo $ "Build completed:" <+> viaShow rep
  return rep

check :: GH.Source -> Text -> Worker Job.Result
check src@GH.Source{..} app = do
  env@Env{..} <- ask
  -- logInfo "Check started"
  rep <- withWorkDir env (GH.RepoRef owner repo) sha $ \workdir -> do
    Build.build workdir ".lodjur/build.nix" src
    RSpec.rspec workdir (Text.unpack app)
  -- logInfo $ "Check completed:" <+> viaShow rep
  return rep
