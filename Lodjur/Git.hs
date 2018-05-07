{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Lodjur.Git where

import           Data.Text (Text)

type Hash = Text

data Revision = Revision
  { unRevision :: Hash
  } deriving (Show, Eq)

data Ref
  = Tag Text
        Revision
  | Branch Text
           Revision
  deriving (Eq, Show)

refRevision :: Ref -> Revision
refRevision (Tag _ rev)    = rev
refRevision (Branch _ rev) = rev
