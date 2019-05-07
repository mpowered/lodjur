{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE NamedFieldPuns         #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeOperators          #-}

module Stream where

import           Control.Concurrent.STM
import           Data.Aeson
import qualified Data.Binary.Builder           as Builder
import           Data.ByteString.Lazy           ( ByteString )
import           Data.Int                       ( Int32 )
import           Data.Text                      ( Text )
import           Network.Wai.EventSource
import           Pipes                   hiding ( Proxy )
import           Servant
import           Servant.EventStream

import           Lodjur.Core                   as Core
import           Lodjur.Database
import           Job
import           Types

type StreamApi = "api" :>
      "watch-jobs" :> ServerSentEvents

streamapijs :: AppM Text
streamapijs = return $ jsForAPI (Proxy :: Proxy StreamApi)

streamapi :: ServerT StreamApi AppM
streamapi  = watchJobs

-- Text should be utf8 safe
textEvent :: ByteString -> ServerEvent
textEvent x = ServerEvent { eventId = Nothing, eventName = Nothing, eventData = [Builder.fromLazyByteString x] }

jsonEvent :: ToJSON a => a -> ServerEvent
jsonEvent = textEvent . encode

recentJobs :: AppM [Job']
recentJobs =
  runDb $ recentRoots 20

job :: Int32 -> AppM Job'
job jobid = do
  j <- runDb $ lookupJob jobid
  maybe (throwError err404) return j

watchJobs :: AppM EventSource
watchJobs = do
  pool <- getEnv Types.envDbPool
  core <- getEnv Types.envCore
  chan <- liftIO $ Core.subscribe core

  let
    go = do
      jobs <- liftIO $ withConnection pool $ \conn -> beam conn (recentRoots 20)
      yield (jsonEvent jobs)
      waitJobEvent
    
    waitJobEvent = do
      event <- liftIO $ atomically $ readTChan chan
      case event of
        Core.JobSubmitted -> return ()
        Core.JobUpdated   -> return ()
        _                 -> waitJobEvent
    
  return $ eventSource go
