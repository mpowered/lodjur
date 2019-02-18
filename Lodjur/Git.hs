module Lodjur.Git where

import           Data.Text (Text)
import           Data.Time.Clock            (UTCTime)

type Hash = Text

data Revision = Revision
  { revisionHash :: Hash
  , revisionTime :: UTCTime
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
