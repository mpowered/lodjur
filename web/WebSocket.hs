{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

module WebSocket where

import           Control.Monad.Reader
import           Lodjur.Core
import qualified Lodjur.Core.Websocket         as Websocket
import           Servant
import           Servant.API.WebSocket
import Types

websocket :: ServerT WebSocketPending AppM
websocket = server
 where
  server pc = do
    core <- asks envCore
    liftIO $ Websocket.serverApp (coreEnv core) pc