module Env where

import           Config
import qualified Git

data Env = Env
  { gitEnv      :: Git.Env
  , buildCfg    :: BuildConfig
  }
