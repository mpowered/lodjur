{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}

module GitHub.Data.Installations where

import GitHub.Auth               (Token)
import GitHub.Data.Id            (Id)
import GitHub.Internal.Prelude
import Data.Text.Encoding        (encodeUtf8)
import Prelude ()

data Installation = Installation
    { installationId   :: !(Id Installation)
    }
  deriving (Show, Data, Typeable, Eq, Ord, Generic)

instance NFData Installation where rnf = genericRnf
instance Binary Installation

data AccessToken = AccessToken
    { accessToken          :: !Token
    , accessTokenExpiresAt :: !(Maybe UTCTime)
    }
  deriving (Show, Data, Typeable, Eq, Ord, Generic)

instance NFData AccessToken where rnf = genericRnf
instance Binary AccessToken

instance FromJSON AccessToken where
    parseJSON = withObject "AccessToken" $ \o -> AccessToken
        <$> (encodeUtf8 <$> o .: "token")
        <*> o .:? "expires_at"

