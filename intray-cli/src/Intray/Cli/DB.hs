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

import Data.Time
import Database.Persist.Sql
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

    -- Server identifier
    identifier ItemUUID Maybe

    deriving Show
    deriving Eq
    deriving Generic
|]

instance Validity ClientItem
