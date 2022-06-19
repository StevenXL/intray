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
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Intray.Web.Server.DB where

import Database.Persist.Sql
import Database.Persist.TH
import Intray.Client
import Intray.Web.Server.Persistence ()

share
  [mkPersist sqlSettings, mkMigrate "migrateLoginCache"]
  [persistLowerCase|

UserToken
    name Username
    token Token

    UniqueUserToken name

    deriving Show
    deriving Eq
    deriving Generic
|]
