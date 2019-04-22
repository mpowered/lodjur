{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeOperators          #-}

module Api where

import           Data.Text (Text)
import           Servant
import           Servant.JS
import           Types

type Api = "api" :>
 (    "start"  :> Post '[JSON] ()
 :<|> "action" :> ReqBody '[JSON] () :> Post '[JSON] ()
 )

apijs :: AppM Text
apijs = return $ jsForAPI (Proxy :: Proxy Api) jquery

api :: ServerT Api AppM
api
    = start
 :<|> act

start :: AppM ()
start = return ()

act :: () -> AppM ()
act _ = return ()