{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE FlexibleInstances    #-}

module Types where

import Control.Monad.Log      ( LoggingT )
import Control.Monad.Reader   ( ReaderT , runReaderT , asks, liftIO )
import Data.Aeson             ( FromJSON, ToJSON )
import Data.Int               ( Int64 )
import Data.Text              ( Text )
import Data.Time              ( UTCTime )
import GHC.Generics           ( Generic )
import Servant                ( Handler )
import Servant.Auth.Server
import Lodjur.Core            ( Core )
import Lodjur.Database        ( DbPool, Pg, withConnection, beam )
import Lodjur.Logging

import GithubAuth

type AppM = ReaderT Env (LoggingT LogMsg Handler)

data Env = Env
  { envLogTarget          :: LogTarget
  , envGithubAppId        :: !Int
  , envGithubClientId     :: !Text
  , envGithubClientSecret :: !Text
  , envGHSettings         :: !(GHSettings AuthUser)
  , envCore               :: Core
  , envDbPool             :: DbPool
  }

data AuthUser = AuthUser
  { authUserId      :: Int64
  , authUserName    :: Text
  , authUserAvatar  :: Maybe Text
  , authUserExpires :: UTCTime
  } deriving (Eq, Show, Read, Generic, FromJSON, ToJSON, FromJWT, ToJWT)

runApp :: Env -> AppM a -> Handler a
runApp env = runLogging (envLogTarget env) . flip runReaderT env

getEnv :: (Env -> a) -> AppM a
getEnv = asks

runDb :: Pg a -> AppM a
runDb a = do
  pool <- getEnv Types.envDbPool
  liftIO $ withConnection pool $ \conn -> beam conn a