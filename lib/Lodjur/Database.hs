{-# LANGUAGE FlexibleContexts #-}

module Lodjur.Database
  ( module Lodjur.Database.Schema
  , module Data.Pool
  , module Database.Beam
  , module Database.Beam.Backend.SQL
  , module Database.Beam.Backend.SQL.BeamExtensions
  , Connection
  , ConnectInfo(..)
  , Pg
  , DbPool
  , withConnection
  )
where

import           Data.Pool
import           Database.Beam
import           Database.Beam.Backend.SQL
import           Database.Beam.Backend.SQL.BeamExtensions
import           Database.Beam.Postgres         ( Connection, ConnectInfo(..), Pg )
import           Control.Monad.Trans.Control
import           Lodjur.Database.Schema

type DbPool = Pool Connection

withConnection :: MonadBaseControl IO m => DbPool -> (Connection -> m a) -> m a
withConnection = withResource