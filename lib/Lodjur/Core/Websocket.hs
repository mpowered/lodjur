{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Lodjur.Core.Websocket where

import           Control.Concurrent
import           Control.Concurrent.Async
import           Control.Concurrent.STM
import           Control.Monad
import           Control.Monad.IO.Class
import           Data.Aeson
import qualified Data.ByteString               as B
import qualified Data.ByteString.Lazy          as LB
import qualified Data.ByteString.Lazy.Char8    as C8
import           Data.Default.Class
import           GHC.Generics
import           Control.Exception
import qualified Network.Connection            as NC
import           Network.WebSockets            as WS
import           Network.WebSockets.Stream
import           Servant                       (Server)
import           Servant.API.WebSocket

import           Lodjur.Core.Types
import qualified Lodjur.Internal.JSON          as JSON
import           Lodjur.Job                    as Job

data HandshakeRequest
  = Greet
  deriving (Show, Eq, Ord, Generic)

instance ToJSON HandshakeRequest where
  toEncoding = genericToEncoding JSON.options

instance FromJSON HandshakeRequest where
  parseJSON = genericParseJSON JSON.options

data HandshakeReply
  = Register
  deriving (Show, Eq, Ord, Generic)

instance ToJSON HandshakeReply where
  toEncoding = genericToEncoding JSON.options

instance FromJSON HandshakeReply where
  parseJSON = genericParseJSON JSON.options

recvMsg :: (FromJSON a, MonadIO m) => WS.Connection -> m a
recvMsg conn = liftIO $ do
  d <- receiveData conn
  putStrLn $ "<- " ++ C8.unpack d
  case decode' d of
    Nothing  -> throwIO WebsocketError
    Just msg -> return msg

sendMsg :: (ToJSON a, MonadIO m) => WS.Connection -> a -> m ()
sendMsg conn msg = liftIO $ do
  sendTextData conn $ encode msg
  putStrLn $ "-> " ++ C8.unpack (encode msg)

server :: Env -> Server WebSocketPending
server env = liftIO . serverApp env

serverApp :: Env -> ServerApp
serverApp env pending = if validRequest (pendingRequest pending)
  then accept pending env
  else reject pending
  where validRequest _ = True   -- TODO move the validator to env

reject :: PendingConnection -> IO ()
reject pending = rejectRequest pending "Authorization failed"

accept :: PendingConnection -> Env -> IO ()
accept pending Env {..} = do
  conn <- acceptRequest pending
  serverHandshake conn
  putStrLn "Worker connected"
  forkPingThread conn 30
  inprogress <- newEmptyTMVarIO
  catch
    (concurrently_ (sendloop conn inprogress) (recvloop conn inprogress))
    (\(SomeException e) -> putStrLn $ "Exception in worker server: " ++ show e)
  putStrLn "Worker disconnected"
  atomically $ do
    r <- tryTakeTMVar inprogress
    case r of
      Just req@(_, a) -> do
        unGetTQueue envJobQueue req
        writeTQueue envReplyQueue (Requeued, a)
      Nothing ->
        return ()
 where
  sendloop conn inprogress = forever $ do
    req <- atomically $ do
      (req, a) <- readTQueue envJobQueue
      putTMVar inprogress (req, a)
      writeTQueue envReplyQueue (Started, a)
      return req
    sendMsg conn req

  recvloop conn inprogress = forever $ do
    res <- recvMsg conn
    atomically $
      case res of
        Concluded{} -> do
          (_, a) <- takeTMVar inprogress
          writeTQueue envReplyQueue (res, a)
        _ -> do
          (_, a) <- readTMVar inprogress
          writeTQueue envReplyQueue (res, a)

serverHandshake :: WS.Connection -> IO ()
serverHandshake conn = do
  sendMsg conn Greet
  Register <- recvMsg conn
  return ()

clientHandshake :: WS.Connection -> IO ()
clientHandshake conn = do
  Greet <- recvMsg conn
  sendMsg conn Register

runClientApp :: ConnectInfo -> ClientApp a -> IO a
runClientApp ci app = do
  ctx <- NC.initConnectionContext
  nc  <- NC.connectTo ctx params
  s   <- makeConnectionStream nc
  runClientWithStream s fullhost (connectPath ci) opts [] app
 where
  port = toInteger (connectPort ci)
  fullhost =
    if port == 80 then connectHost ci else connectHost ci ++ ":" ++ show port
  opts   = defaultConnectionOptions
  params = NC.ConnectionParams
    { connectionHostname  = connectHost ci
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

  s Nothing   = NC.connectionClose conn `catch` \(_ :: IOException) -> return ()
  s (Just bs) = NC.connectionPut conn (LB.toStrict bs)

runClient :: ConnectInfo -> (Job.Request -> Chan Job.Reply -> IO Job.Result) -> IO ()
runClient ci handler =
  catch
    (runClientApp ci clientApp)
    (\ConnectionClosed -> putStrLn "Disconnected from server")
 where
  clientApp conn = do
    clientHandshake conn
    s <- newChan
    r <- newEmptyMVar
    withAsync (clientloop s r) (networking conn s r)

  clientloop s r = forever $ do
    req <- takeMVar r
    rep <- handler req s `catch` \(e :: IOException) -> do
      putStrLn $ "Worker threw an excaption: " ++ show e
      return $ Job.Result Job.Failure Nothing [] Nothing
    writeChan s (Concluded rep)

  networking conn s r _a = concurrently_ (sendloop conn s) (recvloop conn r)

  recvloop conn r = forever $ do
    req <- recvMsg conn
    putMVar r req

  sendloop conn s = forever $ do
    rep <- readChan s
    sendMsg conn rep
