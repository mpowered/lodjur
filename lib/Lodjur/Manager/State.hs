module Lodjur.Manager.State where

import           Control.Concurrent.STM
import qualified Data.HashMap.Strict           as HM

import           Lodjur.Manager.Client
import qualified Lodjur.Manager.Messages       as Msg

data State a = State
  { registeredClients   :: TVar (HM.HashMap ClientId (Client a))
  , requestQueue        :: TQueue (Msg.Request, a)
  , replyQueue          :: TQueue (Msg.Reply, a)
  }
