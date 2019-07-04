{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE FlexibleInstances    #-}

module Types where

import Control.Monad.Reader   ( ReaderT , runReaderT , asks, liftIO )
import Data.Aeson             ( FromJSON, ToJSON )
import Data.Int               ( Int64 )
import Data.Text              ( Text )
import GHC.Generics           ( Generic )
import Servant                ( Handler )
import Servant.Auth.Server
import Lodjur.Core            ( Core )
import Lodjur.Database        ( DbPool, Pg, withConnection, beam )

import GithubAuth

type AppM = ReaderT Env Handler

data Env = Env
  { envGithubAppId        :: !Int
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
  } deriving (Eq, Show, Read, Generic, FromJSON, ToJSON, FromJWT, ToJWT)

runApp :: Env -> AppM a -> Handler a
runApp = flip runReaderT

getEnv :: (Env -> a) -> AppM a
getEnv = asks

runDb :: Pg a -> AppM a
runDb a = do
  pool <- getEnv Types.envDbPool
  liftIO $ withConnection pool $ \conn -> beam conn a