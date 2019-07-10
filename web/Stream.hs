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
import           Control.Monad
import           Data.Aeson
import qualified Data.Binary.Builder           as Builder
import           Data.ByteString.Lazy           ( ByteString )
import           Data.Int                       ( Int32 )
import           Data.Text                      ( Text )
import           Lucid
import           Network.Wai.EventSource
import           Pipes                   hiding ( Proxy )
import           Servant
import           Servant.EventStream

import           Job
import           Lodjur.Core                   as Core
import           Types

type StreamApi = "api" :>
 (    "jobs" :> "watch" :> ServerSentEvents
 :<|> "job" :> Capture "jobId" Int32 :> "watch" :> ServerSentEvents
 :<|> "job" :> Capture "jobId" Int32 :> "watch" :> "logs" :> ServerSentEvents
 )

streamapiAsJS :: Text
streamapiAsJS = jsForAPI (Proxy :: Proxy StreamApi)

streamapi :: ServerT StreamApi AppM
streamapi
    = watchJobs
 :<|> watchJob
 :<|> watchJobLogs

-- Text should be utf8 safe
textEvent :: ByteString -> ServerEvent
textEvent x = ServerEvent { eventId = Nothing, eventName = Nothing, eventData = [Builder.fromLazyByteString x] }

jsonEvent :: ToJSON a => a -> ServerEvent
jsonEvent = textEvent . encode

htmlEvent :: ToHtml a => a -> ServerEvent
htmlEvent = textEvent . renderBS . toHtml

watchJobs :: AppM EventSourceHdr
watchJobs = do
  core <- getEnv Types.envCore
  chan <- liftIO $ Core.subscribe core

  return $
    eventSource $
      forever $ do
        event <- liftIO $ atomically $ readTChan chan
        case event of
          JobSubmitted {} -> yield (jsonEvent event)
          JobUpdated {}   -> yield (jsonEvent event)
          LogsUpdated {}  -> return ()

watchJob :: Int32 -> AppM EventSourceHdr
watchJob jobid = do
  core <- getEnv Types.envCore
  chan <- liftIO $ Core.subscribe core

  return $
    eventSource $
      forever $ do
        event <- liftIO $ atomically $ readTChan chan
        when (eventJobId event == jobid) $
          case event of
            JobSubmitted {} -> yield (jsonEvent event)
            JobUpdated {}   -> yield (jsonEvent event)
            LogsUpdated {}  -> return ()

watchJobLogs :: Int32 -> AppM EventSourceHdr
watchJobLogs jobid = do
  core <- getEnv Types.envCore
  chan <- liftIO $ Core.subscribe core
  logs <- runDb $ jobLogLines jobid

  return $
    eventSource $ do
      yield (htmlEvent logs)
      forever $ do
        event <- liftIO $ atomically $ readTChan chan
        when (eventJobId event == jobid) $
          case event of
            JobSubmitted {} -> return ()
            JobUpdated {}   -> return ()
            LogsUpdated _ logtxt  -> yield (htmlEvent $ LogLine logtxt)
