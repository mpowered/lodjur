{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
module Lodjur.User where

import           Data.Hashable
import           Data.Text     (Text)
import           GHC.Generics

newtype UserId = UserId { unUserId :: Text } deriving (Show, Eq, Generic, Hashable)

newtype User = User
  { userId   :: UserId
  } deriving (Show)
