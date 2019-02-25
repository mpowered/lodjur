{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE StandaloneDeriving         #-}

module GitHub.Data.Installations where

import GitHub.Data.Definitions
import GitHub.Data.Apps          (App)
import GitHub.Data.Id            (Id)
import GitHub.Data.Repos         (Repo)
import GitHub.Data.URL           (URL)
import GitHub.Internal.Prelude
import Prelude ()

data Installation = Installation
    { installationId                :: !(Id Installation)
    , installationAccount           :: !SimpleOwner
    , installationAccessTokensUrl   :: !(Maybe URL)
    , installationRepositoriesUrl   :: !(Maybe URL)
    , installationHtmlUrl           :: !(Maybe URL)
    , installationAppId             :: !(Id App)
    }
    -- TODO permissions
    -- TODO events
  deriving (Show, Data, Typeable, Eq, Ord, Generic)

instance NFData Installation where rnf = genericRnf
instance Binary Installation

instance FromJSON Installation where
    parseJSON = withObject "Installation" $ \o -> Installation
        <$> o .: "id"
        <*> o .: "account"
        <*> o .:?"access_tokens_url"
        <*> o .:?"repositories_url"
        <*> o .:?"html_url"
        <*> o .: "app_id"

newtype Installations = Installations
    { installations :: Vector Installation
    }
  deriving (Show, Data, Typeable, Eq, Ord, Generic)

instance NFData Installations where rnf = genericRnf
instance Binary Installations
deriving instance Semigroup Installations
deriving instance Monoid Installations

instance FromJSON Installations where
    parseJSON = withObject "Installations" $ \o -> Installations
        <$> o .: "installations"

newtype InstallationRepositories = InstallationRepositories
    { installationRepositories :: Vector Repo
    }
  deriving (Show, Data, Typeable, Eq, Ord, Generic)

instance NFData InstallationRepositories where rnf = genericRnf
instance Binary InstallationRepositories
deriving instance Semigroup InstallationRepositories
deriving instance Monoid InstallationRepositories

instance FromJSON InstallationRepositories where
    parseJSON = withObject "InstallationRepositories" $ \o -> InstallationRepositories
        <$> o .: "repositories"
