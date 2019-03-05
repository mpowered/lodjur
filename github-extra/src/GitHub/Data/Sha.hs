{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}

module GitHub.Data.Sha where

import GitHub.Internal.Prelude
import Prelude ()

newtype Sha = Sha { untagSha :: Text }
  deriving (Show, Data, Typeable, Eq, Ord, Generic)

instance NFData Sha where rnf = genericRnf
instance Binary Sha
instance Hashable Sha where
    hashWithSalt salt (Sha l) = hashWithSalt salt l
instance IsString Sha where
    fromString = Sha . fromString

instance FromJSON Sha where
    parseJSON = withText "Sha" (pure . Sha)

instance ToJSON Sha where
    toJSON = toJSON . untagSha
