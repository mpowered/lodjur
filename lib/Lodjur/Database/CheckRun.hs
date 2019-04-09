{-# LANGUAGE RankNTypes #-}

module Lodjur.Database.CheckRun where

import           Lodjur.Database.Internal
import qualified Database.Beam.Postgres.Full   as Pg
import           Database.Beam.Postgres.Syntax

insertCheckRuns :: Connection -> [CheckRun] -> IO ()
insertCheckRuns conn rs =
  beam conn
    $ runInsert
    $ insert (dbCheckRuns db) (insertValues rs)

insertCheckRunsE
  :: Connection
  -> (forall a . [CheckRunT (QExpr PgExpressionSyntax a)])
  -> IO ()
insertCheckRunsE conn rs =
  beam conn
    $ runInsert
    $ insert (dbCheckRuns db) (insertExpressions rs)

updateCheckRun :: Connection -> CheckRun -> IO ()
updateCheckRun conn s =
  beam conn
    $ runUpdate
    $ save (dbCheckRuns db) s

upsertCheckRun :: Connection -> CheckRun -> IO ()
upsertCheckRun conn s =
  beam conn
    $ runInsert
    $ Pg.insert (dbCheckRuns db) (insertValues [s])
    $ Pg.onConflict (Pg.conflictingFields primaryKey) Pg.onConflictSetAll
