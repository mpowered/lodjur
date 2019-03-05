{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}

module GitHub.Data.Apps where

import Data.Text.Encoding        (encodeUtf8)
import GitHub.Auth               (Token)
import GitHub.Data.Definitions
import GitHub.Data.Id            (Id)
import GitHub.Data.Name          (Name)
import GitHub.Data.URL           (URL)
import GitHub.Internal.Prelude
import Prelude ()

data App = App
    { appId             :: !(Id App)
    , appOwner          :: !SimpleOwner
    , appName           :: !(Name App)
    , appDescription    :: !(Maybe Text)
    , appExternalUrl    :: !(Maybe URL)
    , appHtmlUrl        :: !(Maybe URL)
    , appCreatedAt      :: !(Maybe UTCTime)
    , appUpdatedAt      :: !(Maybe UTCTime)
    }
  deriving (Show, Data, Typeable, Eq, Ord, Generic)

instance NFData App where rnf = genericRnf
instance Binary App

instance FromJSON App where
    parseJSON = withObject "App" $ \o -> App
        <$> o .: "id"
        <*> o .: "owner"
        <*> o .: "name"
        <*> o .:?"description"
        <*> o .:?"external_url"
        <*> o .:?"html_url"
        <*> o .:?"created_at"
        <*> o .:?"updated_at"

data AppRef = AppRef
    { appRefId    :: !(Id App)
    , appRefName  :: !(Name App)
    }
  deriving (Show, Data, Typeable, Eq, Ord, Generic)

instance NFData AppRef where rnf = genericRnf
instance Binary AppRef

instance FromJSON AppRef where
    parseJSON = withObject "AppRef" $ \o -> AppRef
        <$> o .: "id"
        <*> o .: "name"

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

