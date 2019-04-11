module Lodjur.Manager.Client where

import           Control.Concurrent.STM
import           Data.Text
import qualified Lodjur.Manager.Messages       as Msg
import           Network.WebSockets             ( Connection )

type ClientId = Text

data Client a = Client
  { clientId            :: ClientId
  , clientConnection    :: Connection
  , clientRequest       :: TMVar (Msg.Request, a)
  }
