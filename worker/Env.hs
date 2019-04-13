module Env where

import           Config
import qualified Git

data Env = Env
  { workDir     :: FilePath
  , gitEnv      :: Git.Env
  , buildCfg    :: BuildConfig
  }
