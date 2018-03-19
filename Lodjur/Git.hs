{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Lodjur.Git where

import           Data.String
import           Data.Text (Text)

newtype Tag =
  Tag { unTag :: Text }
  deriving (Eq, Show, IsString)

