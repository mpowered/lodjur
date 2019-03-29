module Base where

import           Control.Concurrent
import           Data.ByteString              (ByteString)
import           Data.Text                    (Text)
import qualified Database.Redis               as Redis
import qualified GitHub.Extra                 as GH
import           Network.HTTP.Client
import           URI.ByteString
import qualified Web.JWT                      as JWT
import           Web.Spock

import           User

data Env = Env
  { envGithubRepos                      :: ![Text]
  , envGithubSecretToken                :: !ByteString
  , envGithubAppId                      :: !Int
  , envGithubAppSigner                  :: !JWT.Signer
  , envGithubInstallationId             :: !Int
  , envGithubInstallationAccessToken    :: !(MVar (Maybe GH.AccessToken))
  , envManager                          :: !Manager
  , envRedisConn                        :: !Redis.Connection
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
