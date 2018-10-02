module Lodjur.Web.Base where

import           Data.ByteString                 (ByteString)
import           Data.Text                       (Text)
import           Web.Spock

import           Lodjur.Deployment.Deployer
import           Lodjur.Events.EventLogger
import           Lodjur.Git.GitAgent
import           Lodjur.Git.GitReader
import           Lodjur.Output.OutputLoggers
import           Lodjur.Output.OutputStreamer
import           Lodjur.Process

data Env = Env
  { envDeployer          :: Ref Deployer
  , envEventLogger       :: Ref EventLogger
  , envOutputLoggers     :: Ref OutputLoggers
  , envOutputStreamer    :: Ref OutputStreamer
  , envGitAgent          :: Ref GitAgent
  , envGitReader         :: Ref GitReader
  , envGithubRepos       :: [Text]
  , envGithubSecretToken :: ByteString
  }

data Session = Session

type App = SpockM () Session Env
type Action = SpockAction () Session Env
