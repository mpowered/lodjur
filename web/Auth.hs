{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE TypeFamilies           #-}

module Auth where

import           Data.Aeson (FromJSON, ToJSON)
import           GHC.Generics (Generic)
import           Network.Wai
import           Servant
import           Servant.Auth.Server
import           Servant.Server.Experimental.Auth
import           Web.JWT

data Session = Session
  deriving (Eq, Show, Read, Generic)

instance ToJSON Session
instance ToJWT Session
instance FromJSON Session
instance FromJWT Session
type CookieAuth = AuthProtect "cookie-auth" 

type instance AuthServerData CookieAuth = Session

authHandler :: AuthHandler Request Session
authHandler = mkAuthHandler handler
  where
    handler = undefined