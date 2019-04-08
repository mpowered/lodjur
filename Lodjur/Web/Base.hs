{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Lodjur.Web.Base where

import           Control.Monad.Trans          (MonadTrans (..))
import           Control.Monad.Log            (LoggingT (..), runLoggingT)
import           Control.Monad.Reader         (ReaderT (..))
import           Data.ByteString              (ByteString)
import           Data.Text                    (Text)
import           Network.HTTP.Client
import           URI.ByteString
import           Web.Spock
import           Web.Spock.Core               (SpockCtxT)
import           Web.Spock.Routing            (RouteM (..))

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
  }

data Session = Session
  { currentUser :: Maybe User
  , continueTo  :: Maybe (URIRef Relative)
  , oauthState  :: Maybe Text
  }

emptySession :: Session
emptySession = Session Nothing Nothing Nothing

type Info = ()

newtype Spook ctx m a = Spook
  { unSpook :: LoggingT Info (SpockCtxT ctx m) a
  } deriving (Functor, Applicative, Monad)

runSpook :: Monad m => Spook ctx m a -> SpockCtxT ctx m a
runSpook = flip runLoggingT noLogging . unSpook
  where
    noLogging _ = return ()

liftSpook :: Monad m => SpockCtxT ctx m a -> Spook ctx m a
liftSpook = Spook . lift

instance RouteM Spook where
  addMiddleware   = liftSpook . addMiddleware
  wireAny m       = liftSpook . wireAny m
  wireRoute s p a = liftSpook $ wireRoute s p a

  withPrehook a (Spook m) = Spook $ LoggingT $ ReaderT $ \h ->
    withPrehook a $ runLoggingT m (_ . h)

type App = Spook () (WebStateM () Session Env)
type Action = SpockAction () Session Env

-- Specialized types:
_ = () where
  _ = liftSpook :: SpockM () Session Env a -> App a
  _ = runSpook  :: App a -> SpockM () Session Env a
