module Env where

import qualified Database.Redis               as Redis

import qualified Build
import qualified Git

data Env = Env
  { workDir     :: FilePath
  , redisConn   :: Redis.Connection
  , gitEnv      :: Git.Env
  , buildCfg    :: Build.Config
  }
