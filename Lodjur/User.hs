{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
module Lodjur.User where

import           Data.Hashable
import           Data.Text     (Text)
import           GHC.Generics

newtype UserId = UserId { unUserId :: Text } deriving (Show, Eq, Generic, Hashable)

data Role = Core | Tester

data User = User
  { userId      :: UserId
  , userRole    :: Role
  }
