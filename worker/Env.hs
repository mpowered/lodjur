module Env where

import           Network.URI

import qualified Build
import qualified Git

data Env = Env
  { workDir     :: FilePath
  , gitEnv      :: Git.Env
  , buildCfg    :: Build.Config
  }
