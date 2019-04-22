{-# LANGUAGE FlexibleContexts      #-}

module WebSocket where

import           Control.Monad.Reader
import           Lodjur.Core
import qualified Lodjur.Core.Websocket         as Websocket
import           Servant
import           Servant.API.WebSocket
import           Types

websocket :: ServerT WebSocketPending AppM
websocket = server
 where
  server pc = do
    core <- asks envCore
    liftIO $ Websocket.serverApp (coreEnv core) pc