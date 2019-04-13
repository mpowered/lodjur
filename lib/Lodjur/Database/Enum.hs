{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}

module Lodjur.Database.Enum where

import           Data.Text                      ( Text )
import qualified Data.Text                     as Text
import           Database.Beam
import           Database.Beam.Backend.SQL
import           Database.Beam.Postgres
import           Database.PostgreSQL.Simple.FromField
import           Database.PostgreSQL.Simple.ToField

import qualified GitHub.Extra                  as GH

-- Types wrapped in DbEnum will be stored in the database
-- using TextEnum to store/fetch values

newtype DbEnum a = DbEnum { unDbEnum :: a }
  deriving (Eq, Ord, Show)

instance (Typeable a, TextEnum a) => FromField (DbEnum a) where
  fromField f mdata = DbEnum <$> fromDbEnumField f mdata

instance TextEnum a => ToField (DbEnum a) where
  toField = toDbEnumField . unDbEnum

instance (TextEnum a, HasSqlValueSyntax be Text) => HasSqlValueSyntax be (DbEnum a) where
  sqlValueSyntax = sqlValueSyntax . enumToText . unDbEnum

instance (BackendFromField be (DbEnum a), BeamBackend be) => FromBackendRow be (DbEnum a)

-- Class for types able to map their values to text

class TextEnum a where
  enumToText :: a -> Text
  enumFromText :: Text -> Maybe a

instance TextEnum GH.CheckStatus where
  enumToText GH.Queued     = "queued"
  enumToText GH.InProgress = "in_progress"
  enumToText GH.Completed  = "completed"

  enumFromText "queued"      = Just GH.Queued
  enumFromText "in_progress" = Just GH.InProgress
  enumFromText "completed"   = Just GH.Completed
  enumFromText _             = Nothing

instance TextEnum GH.Conclusion where
  enumToText GH.Success        = "success"
  enumToText GH.Failure        = "failure"
  enumToText GH.Neutral        = "neutral"
  enumToText GH.Cancelled      = "cancelled"
  enumToText GH.TimedOut       = "timed_out"
  enumToText GH.ActionRequired = "action_required"

  enumFromText "success"         = Just GH.Success
  enumFromText "failure"         = Just GH.Failure
  enumFromText "neutral"         = Just GH.Neutral
  enumFromText "cancelled"       = Just GH.Cancelled
  enumFromText "timed_out"       = Just GH.TimedOut
  enumFromText "action_required" = Just GH.ActionRequired
  enumFromText _                 = Nothing

toDbEnumField :: TextEnum a => a -> Action
toDbEnumField = toField . enumToText

fromDbEnumField :: (Typeable a, TextEnum a) => FieldParser a
fromDbEnumField f mdata = do
  txt <- fromField f mdata
  case enumFromText txt of
    Nothing -> returnError ConversionFailed f (Text.unpack txt)
    Just a  -> return a
