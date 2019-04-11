module Base where

import           Data.ByteString                ( ByteString )
import           Data.Pool
import           Data.Text                      ( Text )
import qualified Database.PostgreSQL.Simple    as Pg
import           Lodjur.GitHub
import qualified Lodjur.Manager                as Work
import qualified Network.HTTP.Client           as HTTP
import           URI.ByteString
import           User
import           Web.Spock

data Env = Env
  { envGithubRepos                      :: ![Text]
  , envGithubSecretToken                :: !ByteString
  , envGithubAppId                      :: !Int
  , envGithubInstallationId             :: !Int
  , envGithubInstallationAccessToken    :: !GitHubToken
  , envHttpManager                      :: !HTTP.Manager
  , envWorkManager                      :: !(Work.Manager Int)
  , envDbPool                           :: !(Pool Pg.Connection)
  }

data Session = Session
  { currentUser :: !(Maybe User)
  , continueTo  :: !(Maybe (URIRef Relative))
  , oauthState  :: !(Maybe Text)
  } deriving (Show)

emptySession :: Session
emptySession = Session Nothing Nothing Nothing

type App = SpockM Pg.Connection Session Env
type Action = SpockAction Pg.Connection Session Env
