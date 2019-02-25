{-# LANGUAGE RankNTypes #-}

module Lodjur.Database.CheckRun where

import           Lodjur.Database.Internal
import           Database.Beam
import qualified Database.Beam.Postgres        as Pg
import qualified Database.Beam.Postgres.Syntax as Pg

insertCheckRuns :: Pg.Connection -> [CheckRun] -> IO ()
insertCheckRuns conn rs =
  beam conn
    $ runInsert
    $ insert (dbCheckRuns db)
    $ insertValues rs

insertCheckRunsE :: Pg.Connection -> (forall a . [CheckRunT (QExpr Pg.PgExpressionSyntax a)]) -> IO ()
insertCheckRunsE conn rs =
  beam conn
    $ runInsert
    $ insert (dbCheckRuns db)
    $ insertExpressions rs
