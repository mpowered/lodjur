{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

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

data AccessToken = AccessToken
    { accessToken          :: !Token
    , accessTokenExpiresAt :: !(Maybe UTCTime)
    }
  deriving (Show, Data, Typeable, Eq, Ord, Generic)

instance NFData AccessToken where rnf = genericRnf
instance Binary AccessToken

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

instance ToJSON App where
    toJSON App {..} = object
        [ "id"            .= appId
        , "owner"         .= appOwner
        , "name"          .= appName
        , "description"   .= appDescription
        , "external_url"  .= appExternalUrl
        , "html_url"      .= appHtmlUrl
        , "created_at"    .= appCreatedAt
        , "updated_at"    .= appUpdatedAt
        ]

instance FromJSON AccessToken where
    parseJSON = withObject "AccessToken" $ \o -> AccessToken
        <$> (encodeUtf8 <$> o .: "token")
        <*> o .:? "expires_at"
