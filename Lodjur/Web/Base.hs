module Lodjur.Web.Base where

import           Control.Concurrent
import           Data.ByteString              (ByteString)
import           Data.Text                    (Text)
import qualified GitHub.Extra                 as GH
import           Network.HTTP.Client
import           URI.ByteString
import qualified Web.JWT                      as JWT
import           Web.Spock

import           Lodjur.Database
import qualified Lodjur.Build                 as Build
import qualified Lodjur.Git                   as Git
-- import           Lodjur.Deployment.Deployer
-- import           Lodjur.Events.EventLogger
-- import           Lodjur.Git.GitAgent
-- import           Lodjur.Git.GitReader
-- import           Lodjur.Output.OutputLoggers
-- import           Lodjur.Output.OutputStreamer
-- import           Lodjur.Process
import           Lodjur.User

data Env = Env
  -- { envDeployer                      :: Ref Deployer
  -- , envEventLogger                   :: Ref EventLogger
  -- , envOutputLoggers                 :: Ref OutputLoggers
  -- , envOutputStreamer                :: Ref OutputStreamer
  -- , envGitAgent                      :: Ref GitAgent
  -- , envGitReader                     :: Ref GitReader
  { envGithubRepos                      :: ![Text]
  , envGithubSecretToken                :: !ByteString
  , envGithubAppId                      :: !Int
  , envGithubAppSigner                  :: !JWT.Signer
  , envGithubInstallationId             :: !Int
  , envGithubInstallationAccessToken    :: !(MVar (Maybe GH.AccessToken))
  , envManager                          :: !Manager
  , envDbPool                           :: !DbPool
  , envGitEnv                           :: !Git.Env
  , envBuildEnv                         :: !Build.Env
  }

data Session = Session
  { currentUser :: !(Maybe User)
  , continueTo  :: !(Maybe (URIRef Relative))
  , oauthState  :: !(Maybe Text)
  } deriving (Show)

emptySession :: Session
emptySession = Session Nothing Nothing Nothing

type App = SpockM () Session Env
type Action = SpockAction () Session Env
