{-# LANGUAGE RecordWildCards       #-}

module Lodjur.Core where

import           Control.Concurrent.Async
import           Control.Error
import           Control.Monad.IO.Class         ( liftIO )
import           Control.Monad.Reader
import           Data.Bifunctor
import           Data.Pool
import           Data.Time.Clock                ( getCurrentTime )
import qualified GitHub                        as GH
import qualified GitHub.Extra                  as GH
import qualified GitHub.Endpoints.Checks       as GH
import qualified Lodjur.Database               as Db
import qualified Lodjur.Database.CheckRun      as Db
import           Lodjur.GitHub
import qualified Lodjur.Manager                as Mgr
import qualified Lodjur.Manager.Messages       as Msg
import qualified Network.HTTP.Client           as HTTP
import           Network.WebSockets             ( ServerApp )

data Core = Core
  { coreReplyHandler    :: Async ()
  , coreEnv             :: Env
  }

startCore :: GitHubToken -> HTTP.Manager -> Pool Db.Connection-> IO Core
startCore envGithubInstallationAccessToken envHttpManager envDbPool = do
  envWorkManager <- Mgr.newManager
  let coreEnv = Env {..}
  coreReplyHandler <- async (replyHandler coreEnv)
  return Core {..}

cancelCore :: Core -> IO ()
cancelCore core = do
  cancel (coreReplyHandler core)
  Mgr.cancelManager (envWorkManager $ coreEnv core)

coreWSApp :: Core -> ServerApp
coreWSApp = Mgr.managerWebServerApp . envWorkManager . coreEnv

data Env = Env
  { envGithubInstallationAccessToken    :: !GitHubToken
  , envHttpManager                      :: !HTTP.Manager
  , envWorkManager                      :: !(Mgr.Manager Associated)
  , envDbPool                           :: !(Pool Db.Connection)
  }

data Associated = Associated GH.CheckRun Msg.Source
  deriving (Show)

newtype Error
  = GithubError GH.Error
  deriving (Show)

type CoreM = ExceptT Error (ReaderT Env IO)

runCore :: MonadIO m => Env -> CoreM a -> m (Either Error a)
runCore env a = liftIO $ runReaderT (runExceptT a) env

eitherIO :: MonadIO m => (a -> e) -> IO (Either a b) -> ExceptT e m b
eitherIO f a = liftIO a >>= hoistEither . first f

replyHandler :: Env -> IO ()
replyHandler env = forever $ do
  _ <- runCore env $ do
    rep <- waitReply
    handleReply rep
  return ()

request :: Core -> Msg.Request -> IO (Either Error ())
request core = runCore (coreEnv core) . submitRequest

submitRequest :: Msg.Request -> CoreM ()
submitRequest req = do
  Env{..} <- ask
  let src  = Msg.requestSource req
      name = Msg.requestName req
  run <- createCheckRun src name
  liftIO $ Mgr.submitRequest envWorkManager req (Associated run src)

createCheckRun :: Msg.Source -> GH.Name GH.CheckRun -> CoreM GH.CheckRun
createCheckRun Msg.Source{..} name = do
  Env{..} <- ask
  tok <- liftIO $ ensureToken envGithubInstallationAccessToken
  run <- eitherIO GithubError $ GH.createCheckRun (GH.OAuth tok) (GH.simpleOwnerLogin owner) repo $
    (GH.newCheckRun name sha)
      { GH.newCheckRunStatus = Just GH.Queued
      }
  withConn $ \conn -> Db.upsertCheckRun conn (checkrunGHtoDB run)
  return run

updateCheckRun :: Msg.Source -> GH.Id GH.CheckRun -> GH.UpdateCheckRun -> CoreM GH.CheckRun
updateCheckRun Msg.Source{..} runid urun = do
  Env{..} <- ask
  tok <- liftIO $ ensureToken envGithubInstallationAccessToken
  run <- eitherIO GithubError $ GH.updateCheckRun (GH.OAuth tok) (GH.simpleOwnerLogin owner) repo runid urun
  withConn $ \conn -> Db.upsertCheckRun conn (checkrunGHtoDB run)
  return run

handleReply :: (Msg.Reply, Associated) -> CoreM ()
handleReply (rep, Associated run src) = do
  now <- liftIO getCurrentTime
  case rep of
    Msg.Completed concl output -> do
      _run <- updateCheckRun src (GH.checkRunId run) $
        GH.emptyUpdateCheckRun
          { GH.updateCheckRunStatus       = Just GH.Completed
          , GH.updateCheckRunConclusion   = Just concl
          , GH.updateCheckRunOutput       = output
          , GH.updateCheckRunCompletedAt  = Just now
          }
      return ()

    Msg.DependsOn deps output -> do
      _crun <- updateCheckRun src (GH.checkRunId run) $
        GH.emptyUpdateCheckRun
          { GH.updateCheckRunStatus       = Just GH.Completed
          , GH.updateCheckRunConclusion   = Just GH.Neutral
          , GH.updateCheckRunOutput       = output
          , GH.updateCheckRunCompletedAt  = Just now
          }
      mapM_ submitRequest deps

    Msg.Disconnected -> do
      _crun <- updateCheckRun src (GH.checkRunId run) $
        GH.emptyUpdateCheckRun
          { GH.updateCheckRunStatus       = Just GH.Completed
          , GH.updateCheckRunConclusion   = Just GH.Cancelled
          , GH.updateCheckRunCompletedAt  = Just now
          }
      return ()

  -- TODO handle errors, should we retry?

waitReply :: CoreM (Msg.Reply, Associated)
waitReply = do
  Env{..} <- ask
  liftIO $ Mgr.waitReply envWorkManager

withConn :: (Db.Connection -> IO a) -> CoreM a
withConn a = do
  Env{..} <- ask
  liftIO $ withResource envDbPool a
  -- TODO catch database errors?

checkrunGHtoDB :: GH.CheckRun -> Db.CheckRun
checkrunGHtoDB GH.CheckRun{..} =
  Db.CheckRun
    { Db.checkrunId           = GH.untagId checkRunCheckSuiteId
    , Db.checkrunName         = GH.untagName checkRunName
    , Db.checkrunStatus       = Db.DbEnum checkRunStatus
    , Db.checkrunConclusion   = Db.DbEnum <$> checkRunConclusion
    , Db.checkrunStartedAt    = checkRunStartedAt
    , Db.checkrunCompletedAt  = checkRunCompletedAt
    , Db.checkrunCheckSuite   = Db.CheckSuiteKey (GH.untagId checkRunCheckSuiteId)
    }
