module Env where

import           Control.Concurrent
import           Config
import qualified Git
import           Lodjur.Logging
import qualified Lodjur.Manager.Messages       as Msg
import           Network.WebSockets

data Env = Env
  { logTarget   :: LogTarget
  , messageConn :: Connection
  , workDir     :: FilePath
  , gitEnv      :: Git.Env
  , buildCfg    :: BuildConfig
  , request     :: MVar Msg.Request
  , reply       :: MVar Msg.Reply
  }
