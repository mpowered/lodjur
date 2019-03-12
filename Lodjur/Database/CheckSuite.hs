{-# LANGUAGE RankNTypes #-}

module Lodjur.Database.CheckSuite where

import           Lodjur.Database.Internal
import           Database.Beam
import qualified Database.Beam.Postgres        as Pg
import qualified Database.Beam.Postgres.Full   as Pg
import qualified Database.Beam.Postgres.Syntax as Pg

insertCheckSuites :: Pg.Connection -> [CheckSuite] -> IO ()
insertCheckSuites conn ss =
  beam conn
    $ runInsert
    $ insert (dbCheckSuites db) (insertValues ss)

insertCheckSuitesE :: Pg.Connection -> (forall a . [CheckSuiteT (QExpr Pg.PgExpressionSyntax a)]) -> IO ()
insertCheckSuitesE conn ss =
  beam conn
    $ runInsert
    $ insert (dbCheckSuites db) (insertExpressions ss)

updateCheckSuite :: Pg.Connection -> CheckSuite -> IO ()
updateCheckSuite conn s =
  beam conn
    $ runUpdate
    $ save (dbCheckSuites db) s

upsertCheckSuite :: Pg.Connection -> CheckSuite -> IO ()
upsertCheckSuite conn s =
  beam conn
    $ runInsert
    $ Pg.insert (dbCheckSuites db) (insertValues [s])
    $ Pg.onConflict (Pg.conflictingFields primaryKey) Pg.onConflictSetAll

lookupCheckSuite :: Pg.Connection -> Int -> IO (Maybe CheckSuite)
lookupCheckSuite conn sid =
  beam conn
    $ runSelectReturningOne
    $ lookup_ (dbCheckSuites db) (CheckSuiteKey sid)
