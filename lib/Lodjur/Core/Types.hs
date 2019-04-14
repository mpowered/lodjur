module Lodjur.Core.Types
  ( Core(..)
  , Env(..)
  , Associated(..)
  , Event(..)
  , CoreM
  , CoreException(..)
  , runCore

    -- Database
  , Connection

    -- Server
  , ServerApp

    -- Client
  , ConnectInfo(..)
  , PortNumber
  )
where

import           Control.Concurrent.Async
import           Control.Concurrent.STM         ( TQueue, TChan )
import           Control.Exception
import           Control.Monad.Reader
import           Data.Int                       ( Int32 )
import           Data.Pool                      ( Pool )
import           Lodjur.Database                ( Connection )
import qualified Lodjur.GitHub                 as GH
import qualified Lodjur.Job                    as Job
import qualified Network.HTTP.Client           as HTTP
import           Network.Socket                 ( PortNumber )
import           Network.WebSockets             ( ServerApp )

data Core = Core
  { coreReplyHandler    :: Async ()
  , coreWebSocketApp    :: ServerApp
  , coreEnv             :: Env
  }

data Env = Env
  { envGithubInstallationAccessToken    :: !GH.GitHubToken
  , envHttpManager                      :: !HTTP.Manager
  , envDbPool                           :: !(Pool Connection)
  , envJobQueue                         :: !(TQueue (Job.Request, Associated))
  , envReplyQueue                       :: !(TQueue (Job.Reply, Associated))
  , envEventChan                        :: !(TChan Event)
  }

data Event = JobSubmitted | JobUpdated
  deriving (Show, Eq, Ord)

data Associated
  = Associated
    { lodjurJobId   :: Int32
    , githubRun     :: GH.CheckRun
    , githubSource  :: GH.Source
    }
  deriving (Show, Eq, Ord)

data ConnectInfo = ConnectInfo
  { connectSecure       :: Bool     -- Use TLS
  , connectHost         :: String
  , connectPort         :: PortNumber
  , connectPath         :: String
  }
  deriving (Show, Eq, Ord)

type CoreM = ReaderT Env IO

data CoreException
  = GithubError GH.Error
  | WebsocketError
  deriving (Show)

instance Exception CoreException

runCore :: MonadIO m => Env -> CoreM a -> m a
runCore env a = liftIO $ runReaderT a env
