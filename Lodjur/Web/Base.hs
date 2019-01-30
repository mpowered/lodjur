module Lodjur.Web.Base where

import           Data.ByteString              (ByteString)
import           Data.Text                    (Text)
import           Network.HTTP.Client
import           URI.ByteString
import           Web.Spock

import           Lodjur.Deployment.Deployer
import           Lodjur.Events.EventLogger
import           Lodjur.Git.GitAgent
import           Lodjur.Git.GitReader
import           Lodjur.Output.OutputLoggers
import           Lodjur.Output.OutputStreamer
import           Lodjur.Process
import           Lodjur.User

data Env = Env
  { envDeployer          :: Ref Deployer
  , envEventLogger       :: Ref EventLogger
  , envOutputLoggers     :: Ref OutputLoggers
  , envOutputStreamer    :: Ref OutputStreamer
  , envGitAgent          :: Ref GitAgent
  , envGitReader         :: Ref GitReader
  , envGithubRepos       :: [Text]
  , envGithubSecretToken :: ByteString
  , envManager           :: Manager
  , envRunMode           :: RunMode
  }

data Session = Session
  { currentUser :: Maybe User
  , continueTo  :: Maybe (URIRef Relative)
  , oauthState  :: Maybe Text
  }

emptySession :: Session
emptySession = Session Nothing Nothing Nothing

type App = SpockM () Session Env
type Action = SpockAction () Session Env

data RunMode
  -- | Normal operation: Uses Github to authorize users
  = NormalMode

  -- | Development mode: Bypasses Github authorization and instead uses
  -- "Developer" as the user
  | DevMode
  deriving (Eq, Show)
