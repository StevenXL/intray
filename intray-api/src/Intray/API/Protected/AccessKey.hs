{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Intray.API.Protected.AccessKey
  ( IntrayProtectedAccessKeyAPI,
    IntrayProtectedAccessKeySite (..),
    AccessKeyUUID,
    AccessKeyInfo (..),
    AddAccessKey (..),
    AccessKeyCreated (..),
    PostAddAccessKey,
    GetAccessKey,
    GetAccessKeys,
    DeleteAccessKey,
    module Data.UUID.Typed,
  )
where

import Data.UUID.Typed
import Import
import Intray.API.Protected.AccessKey.Types
import Intray.API.Types
import Servant.API

type IntrayProtectedAccessKeyAPI = ToServantApi IntrayProtectedAccessKeySite

data IntrayProtectedAccessKeySite route = IntrayProtectedAccessKeySite
  { postAddAccessKey :: !(route :- PostAddAccessKey),
    getAccessKey :: !(route :- GetAccessKey),
    getAccessKeys :: !(route :- GetAccessKeys),
    deleteAccessKey :: !(route :- DeleteAccessKey)
  }
  deriving (Generic)

type PostAddAccessKey =
  ProtectAPI :> ReqBody '[JSON] AddAccessKey :> Post '[JSON] AccessKeyCreated

type GetAccessKey = ProtectAPI :> Capture "uuid" AccessKeyUUID :> Get '[JSON] AccessKeyInfo

type GetAccessKeys = ProtectAPI :> Get '[JSON] [AccessKeyInfo]

type DeleteAccessKey = ProtectAPI :> Capture "uuid" AccessKeyUUID :> Delete '[JSON] NoContent
