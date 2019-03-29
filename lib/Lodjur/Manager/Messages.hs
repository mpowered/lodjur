{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Lodjur.Manager.Messages where

import           Data.Aeson
import           Data.Text                      ( Text )
import           GHC.Generics
import           GitHub.Extra

data HandshakeRequest
  = Greet
  deriving (Show, Eq, Ord, Generic)

instance ToJSON HandshakeRequest where
  toEncoding = genericToEncoding jsonOptions

instance FromJSON HandshakeRequest where
  parseJSON = genericParseJSON jsonOptions

data HandshakeReply
  = Register
  { clientId    :: !Text
  }
  deriving (Show, Eq, Ord, Generic)

instance ToJSON HandshakeReply where
  toEncoding = genericToEncoding jsonOptions

instance FromJSON HandshakeReply where
  parseJSON = genericParseJSON jsonOptions

data Request
  = Build
  { src         :: !Source
  }
  | Check
  { src         :: !Source
  }
  | Deploy
  { src         :: !Source
  }
  deriving (Show, Eq, Ord, Generic)

instance ToJSON Request where
  toEncoding = genericToEncoding jsonOptions

instance FromJSON Request where
  parseJSON = genericParseJSON jsonOptions

data Reply
  = Completed
  | Cancelled
  | Disconnected
  deriving (Show, Eq, Ord, Generic)

instance ToJSON Reply where
  toEncoding = genericToEncoding jsonOptions

instance FromJSON Reply where
  parseJSON = genericParseJSON jsonOptions

data Source
  = Source
  { sha         :: !Sha
  , owner       :: !Text
  , repo        :: !Text
  }
  deriving (Show, Eq, Ord, Generic)

instance ToJSON Source where
  toEncoding = genericToEncoding jsonOptions

instance FromJSON Source where
  parseJSON = genericParseJSON jsonOptions

jsonOptions :: Options
jsonOptions = defaultOptions { tagSingleConstructors = True }
