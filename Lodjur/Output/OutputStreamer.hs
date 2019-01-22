{-# LANGUAGE GADTs           #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies    #-}
module Lodjur.Output.OutputStreamer
  ( Output (..)
  , OutputStream (..)
  , OutputStreamer
  , OutputStreamMessage (..)
  , initialize
  ) where

import           Control.Concurrent
import           Control.Monad              (foldM, when)
import           Data.HashMap.Strict        (HashMap)
import qualified Data.HashMap.Strict        as HashMap
import           Data.Maybe                 (isJust)

import           Lodjur.Database            (Connection, DbPool, withConnNoTran)
import           Lodjur.Deployment          (JobId)
import           Lodjur.Output
import qualified Lodjur.Output.Database     as Database
import           Lodjur.Process

data OutputStream
  = NextOutput Output
  | Fence

type OutputChan = Chan OutputStream

data Sub = Sub
  { subChan :: OutputChan
  , subSince :: Maybe Integer
  , subFence :: Maybe Integer
  }

type Subscriptions = HashMap JobId [Sub]

data OutputStreamer = OutputStreamer
  { dbPool :: DbPool
  , threadId :: ThreadId
  , subscriptions :: MVar Subscriptions
  }

initialize :: DbPool -> IO OutputStreamer
initialize dbPool = do
  subscriptions <- newMVar HashMap.empty
  threadId <- forkIO (idler dbPool subscriptions)
  return OutputStreamer {..}

data OutputStreamMessage r where
  -- Public messages:
  SubscribeOutputLog :: JobId -> Maybe Integer -> OutputChan -> OutputStreamMessage Async
  UnsubscribeOutputLog :: JobId -> OutputChan -> OutputStreamMessage (Sync ())

instance Process OutputStreamer where
  type Message OutputStreamer = OutputStreamMessage

  receive _self (streamer, SubscribeOutputLog jobId since chan) = do
    subs <- takeMVar (subscriptions streamer)
    putMVar (subscriptions streamer) $
      HashMap.insertWith (++) jobId [Sub chan since Nothing] subs
    return streamer

  receive _self (streamer, UnsubscribeOutputLog jobId chan) = do
    subs <- takeMVar (subscriptions streamer)
    putMVar (subscriptions streamer) $
      HashMap.update (dropChan chan) jobId subs
    return (streamer, ())
   where
    dropChan c ss =
      case filter ((/= c) . subChan) ss of
        [] -> Nothing
        ss' -> Just ss'

  terminate _ = return ()

idler :: DbPool -> MVar Subscriptions -> IO ()
idler pool var = do
  subs <- readMVar var
  if HashMap.null subs
    then
      threadDelay 1000000
    else
      withConnNoTran pool $ \conn -> do
        Database.listen conn
        distributer conn var
        Database.unlisten conn
  idler pool var

distributer :: Connection -> MVar Subscriptions -> IO ()
distributer conn var = do
  subs <- takeMVar var
  if HashMap.null subs
    then
      putMVar var subs
    else do
      n <- Database.outputNotification conn
      case n of
        Just jobId ->
          notifyJob subs jobId >>= putMVar var
        Nothing -> do
          foldM notifyJob subs (HashMap.keys subs) >>= putMVar var
          threadDelay 1000000
      distributer conn var
 where
  notifyJob subs jobId = do
    let ss = HashMap.lookupDefault [] jobId subs
    ss' <- mapM (notify conn jobId) ss
    return $ HashMap.insert jobId ss' subs

notify :: Connection -> JobId -> Sub -> IO Sub
notify conn jobId Sub{..} = do
  fence <- maybe (Database.nextFence conn jobId Nothing) (return . Just) subFence
  out <- Database.getOutputLogConn conn subSince fence jobId
  writeList2Chan subChan (map NextOutput out)
  when (isJust fence) $
    writeChan subChan Fence
  return Sub { subSince = lastSeen (map outputIndex out)
             , subFence = fence
             , ..
             }
 where
   lastSeen []  = subSince
   lastSeen ts  = Just (maximum ts)
