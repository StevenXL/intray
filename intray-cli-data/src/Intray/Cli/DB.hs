{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans -Wno-name-shadowing #-}

module Intray.Cli.DB where

import Autodocodec
import Data.Aeson (FromJSONKey (..), ToJSONKey (..))
import Data.Functor.Contravariant
import Data.Time
import Database.Persist.Sql as Sql
import Database.Persist.TH
import Intray.Data.Import
import Intray.Data.ItemType
import Intray.Data.ItemUUID

share
  [mkPersist sqlSettings, mkMigrate "clientAutoMigration"]
  [persistLowerCase|

ClientItem
    type ItemType
    contents ByteString
    created UTCTime

    serverIdentifier ItemUUID Maybe
    deleted Bool

    deriving Show
    deriving Eq
    deriving Generic


ShownItem
    item ClientItemId

    UniqueShownItem item -- This means there can only be 0 or 1

    deriving Show
    deriving Eq
    deriving Generic

|]

instance Validity ClientItem

instance ToBackendKey SqlBackend a => HasCodec (Sql.Key a) where
  codec = dimapCodec toSqlKey fromSqlKey codec

instance (PersistEntity a, ToBackendKey SqlBackend a) => ToJSONKey (Sql.Key a) where
  toJSONKey = contramap fromSqlKey toJSONKey

instance (PersistEntity a, ToBackendKey SqlBackend a) => FromJSONKey (Sql.Key a) where
  fromJSONKey = toSqlKey <$> fromJSONKey
