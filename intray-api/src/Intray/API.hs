{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Intray.API
  ( module Intray.Data,
    module Intray.API,
    module Intray.API.Admin,
    module Intray.API.Protected,
    module Intray.API.Types,
    module Data.UUID.Typed,
  )
where

import Data.Aeson as JSON
import Data.UUID.Typed
import Import
import Intray.API.Admin
import Intray.API.Protected
import Intray.API.Types
import Intray.Data
import Servant.API

intrayAPI :: Proxy IntrayAPI
intrayAPI = Proxy

type IntrayAPI = ToServantApi IntraySite

data IntraySite route = IntraySite
  { openSite :: !(route :- ToServantApi IntrayOpenSite),
    adminSite :: !(route :- "admin" :> ToServantApi IntrayAdminSite)
  }
  deriving (Generic)

intrayOpenAPI :: Proxy IntrayOpenAPI
intrayOpenAPI = Proxy

type IntrayOpenAPI = ToServantApi IntrayOpenSite

data IntrayOpenSite route = IntrayOpenSite
  { protectedSite :: !(route :- ToServantApi IntrayProtectedSite),
    publicSite :: !(route :- ToServantApi IntrayPublicSite)
  }
  deriving (Generic)

type IntrayPublicAPI = ToServantApi IntrayPublicSite

data IntrayPublicSite route = IntrayPublicSite
  { postRegister :: !(route :- PostRegister),
    postLogin :: !(route :- PostLogin),
    getPricing :: !(route :- GetPricing),
    postStripeHook :: !(route :- PostStripeHook)
  }
  deriving (Generic)

-- | The order of the items is not guaranteed to be the same for every call.
type PostRegister =
  "register"
    :> ReqBody '[JSON] Registration
    :> Verb 'POST 204 '[JSON] NoContent

type PostLogin =
  "login"
    :> ReqBody '[JSON] LoginForm
    :> Verb 'POST 204 '[JSON] (Headers '[Header "Set-Cookie" Text] NoContent)

type GetPricing =
  "pricing"
    :> Get '[JSON] (Maybe Pricing)

type PostStripeHook =
  "stripe"
    :> ReqBody '[JSON] JSON.Value
    :> Verb 'POST 204 '[JSON] NoContent
