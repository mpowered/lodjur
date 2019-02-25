{-# LANGUAGE FlexibleContexts #-}
module Lodjur.Database
  ( DbPool
  , Connection
  , newPool
  , destroyPool
  , withConn
  , withConnNoTran
  , module Lodjur.Database.Internal
  )
where

import           Control.Monad.IO.Class
import           Data.Pool
import           Data.Time.Clock               (NominalDiffTime)
import           Database.PostgreSQL.Simple
import           Lodjur.Database.Internal

type DbPool = Pool Connection

newPool :: ConnectInfo -> Int -> NominalDiffTime -> Int -> IO DbPool
newPool ci = createPool (connect ci) close

destroyPool :: DbPool -> IO ()
destroyPool = destroyAllResources

withConn :: MonadIO m => DbPool -> (Connection -> IO a) -> m a
withConn pool a = liftIO $ withResource pool $ \conn -> withTransaction conn $ a conn

withConnNoTran :: MonadIO m => DbPool -> (Connection -> IO a) -> m a
withConnNoTran pool a = liftIO $ withResource pool a
