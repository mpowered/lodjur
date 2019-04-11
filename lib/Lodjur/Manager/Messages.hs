{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Lodjur.Manager.Messages where

import           Data.Aeson
import           Data.Text                      ( Text )
import           GHC.Generics
import qualified GitHub                        as GH
import qualified GitHub.Extra                  as GH

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
  { name        :: !(GH.Name GH.CheckRun)
  , src         :: !Source
  }
  | Check
  { name        :: !(GH.Name GH.CheckRun)
  , src         :: !Source
  }
  | Deploy
  { name        :: !(GH.Name GH.CheckRun)
  , src         :: !Source
  }
  deriving (Show, Eq, Ord, Generic)

instance ToJSON Request where
  toEncoding = genericToEncoding jsonOptions

instance FromJSON Request where
  parseJSON = genericParseJSON jsonOptions

data Reply
  = Completed GH.Conclusion (Maybe GH.CheckRunOutput)
  | DependsOn [Request] (Maybe GH.CheckRunOutput)
  | Disconnected
  deriving (Show, Eq, Ord, Generic)

instance ToJSON Reply where
  toEncoding = genericToEncoding jsonOptions

instance FromJSON Reply where
  parseJSON = genericParseJSON jsonOptions

data Source
  = Source
  { sha         :: !GH.Sha
  , owner       :: !GH.SimpleOwner
  , repo        :: !(GH.Name GH.Repo)
  }
  deriving (Show, Eq, Ord, Generic)

instance ToJSON Source where
  toEncoding = genericToEncoding jsonOptions

instance FromJSON Source where
  parseJSON = genericParseJSON jsonOptions

jsonOptions :: Options
jsonOptions = defaultOptions { tagSingleConstructors = True }
