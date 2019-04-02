module Env where

import           Config
import qualified Git
import           Network.WebSockets

data Env = Env
  { logFile     :: FilePath
  , messageConn :: Connection
  , workDir     :: FilePath
  , gitEnv      :: Git.Env
  , buildCfg    :: BuildConfig
  }
