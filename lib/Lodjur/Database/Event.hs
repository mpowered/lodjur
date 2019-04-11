{-# LANGUAGE RankNTypes #-}

module Lodjur.Database.Event where

import           Lodjur.Database.Internal
import           Database.Beam.Postgres.Syntax

insertEvents :: Connection -> [Event] -> IO ()
insertEvents conn es =
  beam conn
    $ runInsert
    $ insert (dbEvents db)
    $ insertValues es

insertEventsE :: Connection -> (forall a . [EventT (QExpr PgExpressionSyntax a)]) -> IO ()
insertEventsE conn es =
  beam conn
    $ runInsert
    $ insert (dbEvents db)
    $ insertExpressions es
