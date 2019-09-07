module Intray.Server.OptParse.Types where

import Import

import Database.Persist.Sqlite

import Web.Stripe.Client as Stripe
import Web.Stripe.Types as Stripe

import Intray.API

type Arguments = (Command, Flags)

type Instructions = (Dispatch, Settings)

newtype Command =
  CommandServe ServeFlags
  deriving (Show, Eq)

data ServeFlags =
  ServeFlags
    { serveFlagPort :: !(Maybe Int)
    , serveFlagDb :: !(Maybe Text)
    , serveFlagAdmins :: ![String]
    , serveFlagStripePlan :: !(Maybe String)
    , serveFlagStripeSecretKey :: !(Maybe String)
    , serveFlagStripePublishableKey :: !(Maybe String)
    }
  deriving (Show, Eq)

data Flags =
  Flags
  deriving (Show, Eq)

data Configuration =
  Configuration
  deriving (Show, Eq)

data Environment =
  Environment
    { envPort :: !(Maybe Int)
    , envStripePlan :: !(Maybe String)
    , envStripeSecretKey :: !(Maybe String)
    , envStripePublishableKey :: !(Maybe String)
    }
  deriving (Show, Eq)

newtype Dispatch =
  DispatchServe ServeSettings
  deriving (Show)

data Settings =
  Settings
  deriving (Show, Eq)

data ServeSettings =
  ServeSettings
    { serveSetPort :: !Int
    , serveSetConnectionInfo :: !SqliteConnectionInfo
    , serveSetAdmins :: ![Username]
    , serveSetMonetisationSettings :: !(Maybe MonetisationSettings)
    }
  deriving (Show)

data MonetisationSettings =
  MonetisationSettings
    { monetisationSetPlan :: !Stripe.PlanId
    , monetisationSetStripeConfig :: !StripeConfig
    , monetisationSetStripePublishableKey :: !Text
    }
  deriving (Show)
