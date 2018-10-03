{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
module Lodjur.User where

import GHC.Generics
import Data.Hashable
import           Data.Text (Text)

newtype UserId = UserId Text deriving (Show, Eq, Generic, Hashable)

data User = User
  { userId   :: UserId
  , fullName :: Text
  }
