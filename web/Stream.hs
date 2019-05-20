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

import           Lodjur.Core                   as Core
import           Lodjur.Database
import           Job
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

watchJobs :: AppM EventSource
watchJobs = do
  core <- getEnv Types.envCore
  chan <- liftIO $ Core.subscribe core

  return $
    eventSource $
      forever $ do
        event <- liftIO $ atomically $ readTChan chan
        yield (jsonEvent event)

watchJob :: Int32 -> AppM EventSource
watchJob jobid = do
  pool <- getEnv Types.envDbPool
  core <- getEnv Types.envCore
  chan <- liftIO $ Core.subscribe core

  let
    go = do
      j <- liftIO $ withConnection pool $ \conn -> beam conn (lookupJob jobid)
      yield undefined -- (htmlEvent j)
      waitJobEvent

    waitJobEvent = do
      event <- liftIO $ atomically $ readTChan chan
      case event of
        (Core.JobSubmitted _) -> return ()
        (Core.JobUpdated _)   -> return ()
        _                     -> waitJobEvent

  return $ eventSource go

watchJobLogs :: Int32 -> AppM EventSource
watchJobLogs jobid = do
  pool <- getEnv Types.envDbPool
  core <- getEnv Types.envCore
  chan <- liftIO $ Core.subscribe core

  let
    go = do
      j <- liftIO $ withConnection pool $ \conn -> beam conn (jobLogsTail jobid)
      yield (htmlEvent j)
      waitJobEvent

    waitJobEvent = do
      event <- liftIO $ atomically $ readTChan chan
      case event of
        Core.LogsUpdated uid
          | uid == jobid -> return ()
          | otherwise    -> waitJobEvent
        _                -> waitJobEvent

  return $ eventSource go
