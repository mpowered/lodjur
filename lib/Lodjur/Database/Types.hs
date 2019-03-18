{-# LANGUAGE TypeFamilies               #-}

module Lodjur.Database.Types where

import           Control.Exception
import           Data.ByteString

class HasKey a where
  data Key a
  redisKey :: Key a -> ByteString

data DatabaseException
  = DecodeError String
  deriving (Show, Eq)

instance Exception DatabaseException
