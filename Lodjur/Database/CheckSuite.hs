{-# LANGUAGE RankNTypes #-}

module Lodjur.Database.CheckSuite where

import           Lodjur.Database.Internal
import           Database.Beam
import qualified Database.Beam.Postgres        as Pg
import qualified Database.Beam.Postgres.Syntax as Pg

insertCheckSuites :: Pg.Connection -> [CheckSuite] -> IO ()
insertCheckSuites conn ss =
  beam conn
    $ runInsert
    $ insert (dbCheckSuites db)
    $ insertValues ss

insertCheckSuitesE :: Pg.Connection -> (forall a . [CheckSuiteT (QExpr Pg.PgExpressionSyntax a)]) -> IO ()
insertCheckSuitesE conn ss =
  beam conn
    $ runInsert
    $ insert (dbCheckSuites db)
    $ insertExpressions ss
