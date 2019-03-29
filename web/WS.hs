{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}

module WS
  ( Manager(..)
  , newManager
  , request
  )
where

import           Control.Concurrent
import           Control.Exception
import           Control.Monad
import           Data.Aeson
import           Data.Text                      ( Text )
import           Network.WebSockets
import qualified Data.HashMap.Strict           as HM
import qualified Lodjur.Manager.Messages       as Msg

type ClientId = Text

data Client = Client
  { clientConnection    :: Connection
  , clientBusy          :: Bool
  , clientRequest       :: MVar Msg.Request
  , clientReply         :: MVar Msg.Reply
  }

data Manager = Manager
  { managerState        :: State
  , managerWebServerApp :: ServerApp
  }

newtype State = State
  { registeredClients   :: MVar (HM.HashMap ClientId Client)
  }

newManager :: IO Manager
newManager = do
  registeredClients <- newMVar HM.empty
  let managerState        = State {..}
      managerWebServerApp = wsapp managerState
  return Manager {..}

request :: Manager -> Msg.Request -> IO Msg.Reply
request mgr req = do
  (clientid, client) <- reserveClient mgr
  putMVar (clientRequest client) req
  rep <- takeMVar (clientReply client)
  releaseClient mgr clientid
  return rep

reserveClient :: Manager -> IO (ClientId, Client)
reserveClient mgr = go
 where
  go = do
    clients <- takeMVar (registeredClients $ managerState mgr)
    let idle = filter (not . clientBusy . snd) (HM.toList clients)
    case headMay idle of
      Just (clientid, client) -> do
        putMVar (registeredClients $ managerState mgr)
          $ HM.insert clientid (client { clientBusy = True }) clients
        return (clientid, client)
      Nothing -> go

  headMay (a : _) = Just a
  headMay []      = Nothing

releaseClient :: Manager -> ClientId -> IO ()
releaseClient mgr clientid = do
  clients <- takeMVar (registeredClients $ managerState mgr)
  let clients' = case HM.lookup clientid clients of
        Just client ->
          HM.insert clientid (client { clientBusy = False }) clients
        Nothing -> clients
  putMVar (registeredClients $ managerState mgr) clients'

wsapp :: State -> ServerApp
wsapp state pending_conn = if validRequest (pendingRequest pending_conn)
  then accept pending_conn state
  else reject pending_conn
  -- where validRequest RequestHead {..} = requestPath == "/manager"
  where validRequest _ = True

data AppError
  = UnexpectedReply
  deriving (Show, Exception)

reject :: PendingConnection -> IO ()
reject pending_conn = rejectRequest pending_conn "Authorization failed"

accept :: PendingConnection -> State -> IO ()
accept pending_conn state = do
  putStrLn "Accepted"
  conn <- acceptRequest pending_conn
  forkPingThread conn 30
  r <- try $ registerClient conn state
  case (r :: Either ConnectionException Client) of
    Left  _      -> return ()
    Right client -> forever $ do
      req <- takeMVar (clientRequest client)
      sendMsg conn req
      msg <- try $ receiveMsg conn
      case (msg :: Either ConnectionException Msg.Reply) of
        Left  _   -> putMVar (clientReply client) Msg.Disconnected
        Right rep -> putMVar (clientReply client) rep

registerClient :: Connection -> State -> IO Client
registerClient conn state = do
  sendMsg conn Msg.Greet
  msg    <- receiveMsg conn
  client <- Client conn False <$> newEmptyMVar <*> newEmptyMVar
  case msg of
    Msg.Register clientid -> do
      clients <- takeMVar (registeredClients state)
      case HM.lookup clientid clients of
        Just old -> sendClose
          (clientConnection old)
          ("Another client with the same name registered" :: Text)
        Nothing -> return ()
      putMVar (registeredClients state) $! HM.insert clientid client clients
  return client

receiveMsg :: FromJSON a => Connection -> IO a
receiveMsg conn = do
  d <- receiveData conn
  putStrLn $ "Recv: " ++ show d
  case decode' d of
    Nothing  -> throwIO UnexpectedReply
    Just msg -> return msg

sendMsg :: ToJSON a => Connection -> a -> IO ()
sendMsg conn msg = do
  putStrLn $ "Send: " ++ show (encode msg)
  sendTextData conn . encode $ msg
