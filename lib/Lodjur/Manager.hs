{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}

module Lodjur.Manager
  ( Manager(..)
  , ConnectInfo(..)
  , newManager
  , request
  , parseManagerURI
  , fromURI
  , runManagerClient
  , sendMsg
  , receiveMsg
  )
where

import           Control.Concurrent
import           Control.Exception
import           Control.Monad
import           Data.Aeson
import qualified Data.ByteString               as B
import qualified Data.ByteString.Lazy          as LB
import           Data.Default.Class
import           Data.Text                      ( Text )
import qualified Network.Connection            as NC
import           Network.Socket                 ( PortNumber )
import qualified Network.URI                   as URI
import           Network.WebSockets      hiding ( runClient )
import           Network.WebSockets.Stream
import qualified Data.HashMap.Strict           as HM
import qualified Lodjur.Manager.Messages       as Msg
import           Text.Read                      ( readMaybe )

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

data ConnectInfo = ConnectInfo
  { connectSecure       :: Bool     -- Use TLS
  , connectHost         :: String
  , connectPort         :: PortNumber
  , connectPath         :: String
  }

newManager :: IO Manager
newManager = do
  registeredClients <- newMVar HM.empty
  let managerState        = State { .. }
      managerWebServerApp = wsapp managerState
  return Manager { .. }

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

parseManagerURI :: String -> Maybe ConnectInfo
parseManagerURI = fromURI <=< URI.parseURI

fromURI :: URI.URI -> Maybe ConnectInfo
fromURI uri = do
  guard $ URI.uriIsAbsolute uri
  connectSecure <- case URI.uriScheme uri of
    "ws:"  -> return False
    "wss:" -> return True
    _     -> mzero
  guard $ URI.uriQuery uri == ""
  guard $ URI.uriFragment uri == ""
  auth <- URI.uriAuthority uri
  guard $ URI.uriUserInfo auth == ""
  let connectHost = URI.uriRegName auth
  connectPort <- readMaybe (dropWhile (== ':') $ URI.uriPort auth)
  let connectPath = URI.uriPath uri
  return ConnectInfo { .. }

runManagerClient :: ConnectInfo -> ClientApp a -> IO a
runManagerClient ci app = do
  ctx <- NC.initConnectionContext
  nc  <- NC.connectTo ctx params
  s   <- makeConnectionStream nc
  runClientWithStream s fullhost (connectPath ci) opts [] app
 where
  port = toInteger (connectPort ci)
  fullhost =
    if port == 80 then connectHost ci else connectHost ci ++ ":" ++ show port
  opts   = defaultConnectionOptions
  params = NC.ConnectionParams { connectionHostname  = connectHost ci
                               , connectionPort      = connectPort ci
                               , connectionUseSecure = secure
                               , connectionUseSocks  = Nothing
                               }
  secure = if connectSecure ci then Just def else Nothing

makeConnectionStream :: NC.Connection -> IO Stream
makeConnectionStream conn = makeStream r s
 where
  r = do
    bs <- NC.connectionGetChunk conn
    return $ if B.null bs then Nothing else Just bs

  s Nothing   = NC.connectionClose conn `Control.Exception.catch` \(_ :: IOException) -> return ()
  s (Just bs) = NC.connectionPut conn (LB.toStrict bs)
