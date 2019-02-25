{-# LANGUAGE RankNTypes #-}

module Lodjur.Database.Event where

import           Lodjur.Database.Internal
import           Database.Beam
import qualified Database.Beam.Postgres        as Pg
import qualified Database.Beam.Postgres.Syntax as Pg

insertEvents :: Pg.Connection -> [Event] -> IO ()
insertEvents conn es =
  beam conn
    $ runInsert
    $ insert (dbEvents db)
    $ insertValues es

insertEventsE :: Pg.Connection -> (forall a . [EventT (QExpr Pg.PgExpressionSyntax a)]) -> IO ()
insertEventsE conn es =
  beam conn
    $ runInsert
    $ insert (dbEvents db)
    $ insertExpressions es
