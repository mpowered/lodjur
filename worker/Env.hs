module Env where

import           Config
import qualified Git
import           Lodjur.Logging
import           Network.WebSockets

data Env = Env
  { logTarget   :: LogTarget
  , messageConn :: Connection
  , workDir     :: FilePath
  , gitEnv      :: Git.Env
  , buildCfg    :: BuildConfig
  }
