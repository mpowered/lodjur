{-# LANGUAGE FlexibleInstances    #-}

module Types where

import Control.Monad.Reader   ( ReaderT , runReaderT , asks, liftIO )
import Servant                ( Handler )
import Lodjur.Core            ( Core )
import Lodjur.Database        ( DbPool, Pg, withConnection, beam )

type AppM = ReaderT Env Handler

data Env = Env
  { envGithubAppId      :: !Int
  , envCore             :: Core
  , envDbPool           :: DbPool
  }

runApp :: Env -> AppM a -> Handler a
runApp = flip runReaderT

getEnv :: (Env -> a) -> AppM a
getEnv = asks

runDb :: Pg a -> AppM a
runDb a = do
  pool <- getEnv Types.envDbPool
  liftIO $ withConnection pool $ \conn -> beam conn a