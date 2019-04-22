module Types where

import           Control.Monad.Reader           ( ReaderT, runReaderT, asks )
import           Servant                        ( Handler )
import           Lodjur.Core                    ( Core )
import           Lodjur.Database                ( DbPool )

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