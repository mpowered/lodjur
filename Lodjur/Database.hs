{-# LANGUAGE FlexibleContexts  #-}
module Lodjur.Database where

import           Data.Pool
import           Data.Time.Clock            (NominalDiffTime)
import           Database.PostgreSQL.Simple

type DbPool = Pool Connection

newPool :: ConnectInfo -> Int -> NominalDiffTime -> Int -> IO DbPool
newPool ci = createPool (connect ci) close

destroyPool :: DbPool -> IO ()
destroyPool = destroyAllResources

withConn :: DbPool -> (Connection -> IO a) -> IO a
withConn pool a = withResource pool $ \conn -> withTransaction conn $ a conn

withConnNoTran :: DbPool -> (Connection -> IO a) -> IO a
withConnNoTran = withResource
