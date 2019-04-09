{-# LANGUAGE RankNTypes #-}

module Lodjur.Database.CheckSuite where

import           Lodjur.Database.Internal
import qualified Database.Beam.Postgres.Full   as Pg
import           Database.Beam.Postgres.Syntax

insertCheckSuites :: Connection -> [CheckSuite] -> IO ()
insertCheckSuites conn ss =
  beam conn
    $ runInsert
    $ insert (dbCheckSuites db) (insertValues ss)

insertCheckSuitesE
  :: Connection
  -> (forall a . [CheckSuiteT (QExpr PgExpressionSyntax a)])
  -> IO ()
insertCheckSuitesE conn ss =
  beam conn
    $ runInsert
    $ insert (dbCheckSuites db) (insertExpressions ss)

updateCheckSuite :: Connection -> CheckSuite -> IO ()
updateCheckSuite conn s =
  beam conn
    $ runUpdate
    $ save (dbCheckSuites db) s

upsertCheckSuite :: Connection -> CheckSuite -> IO ()
upsertCheckSuite conn s =
  beam conn
    $ runInsert
    $ Pg.insert (dbCheckSuites db) (insertValues [s])
    $ Pg.onConflict (Pg.conflictingFields primaryKey) Pg.onConflictSetAll

lookupCheckSuite :: Connection -> Int -> IO (Maybe CheckSuite)
lookupCheckSuite conn sid =
  beam conn
    $ runSelectReturningOne
    $ lookup_ (dbCheckSuites db) (CheckSuiteKey sid)
