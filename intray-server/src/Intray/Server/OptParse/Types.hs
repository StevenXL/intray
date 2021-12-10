{-# LANGUAGE OverloadedStrings #-}

module Intray.Server.OptParse.Types where

import Autodocodec
import Control.Monad.Logger
import Database.Persist.Sqlite
import Import
import Intray.API
import Looper
import qualified Web.Stripe.Client as Stripe
import qualified Web.Stripe.Types as Stripe

data Flags = Flags
  { flagConfigFile :: !(Maybe FilePath),
    flagHost :: !(Maybe String),
    flagPort :: !(Maybe Int),
    flagDb :: !(Maybe Text),
    flagAdmins :: ![String],
    flagFreeloaders :: ![String],
    flagLogLevel :: Maybe LogLevel,
    flagSigningKeyFile :: !(Maybe FilePath),
    flagStripePlan :: !(Maybe String),
    flagStripeSecretKey :: !(Maybe String),
    flagStripePublishableKey :: !(Maybe String),
    flagLooperStripeEventsFetcher :: LooperFlags,
    flagLooperStripeEventsRetrier :: LooperFlags,
    flagMaxItemsFree :: !(Maybe Int)
  }
  deriving (Show, Eq)

data Configuration = Configuration
  { confHost :: !(Maybe String),
    confPort :: !(Maybe Int),
    confDb :: !(Maybe Text),
    confAdmins :: !(Maybe [String]),
    confFreeloaders :: !(Maybe [String]),
    confLogLevel :: !(Maybe LogLevel),
    confSigningKeyFile :: !(Maybe FilePath),
    confMonetisationConfig :: !(Maybe MonetisationConfiguration)
  }
  deriving (Show, Eq)

instance HasCodec Configuration where
  codec =
    object "Configuration" $
      Configuration
        <$> optionalFieldOrNull "host" "The host to serve the api-server on" .= confHost
        <*> optionalFieldOrNull "port" "The port to serve the api-server on" .= confPort
        <*> optionalFieldOrNull "database" "The database file" .= confDb
        <*> optionalFieldOrNull "admins" "The list of usernames that will be considered administrators" .= confAdmins
        <*> optionalFieldOrNull "freeloaders" "The list of usernames that won't have to pay" .= confFreeloaders
        <*> optionalFieldOrNull "log-level" "The minimal log level for log messages" .= confLogLevel
        <*> optionalFieldOrNull "signing-key-file" "The file to store the JWT signing key in" .= confSigningKeyFile
        <*> optionalFieldOrNull "monetisation" "Monetisation configuration. If this is not configured then the server is run for free." .= confMonetisationConfig

data MonetisationConfiguration = MonetisationConfiguration
  { monetisationConfStripePlan :: !(Maybe String),
    monetisationConfStripeSecretKey :: !(Maybe String),
    monetisationConfStripePublishableKey :: !(Maybe String),
    monetisationConfStripeEventsFetcher :: !(Maybe LooperConfiguration),
    monetisationConfStripeEventsRetrier :: !(Maybe LooperConfiguration),
    monetisationConfMaxItemsFree :: !(Maybe Int)
  }
  deriving (Show, Eq)

instance HasCodec MonetisationConfiguration where
  codec =
    object "MonetisationConfiguration" $
      MonetisationConfiguration
        <$> optionalFieldOrNull
          "stripe-plan"
          "The stripe identifier of the stripe plan used to checkout a subscription"
          .= monetisationConfStripePlan
        <*> optionalFieldOrNull "stripe-secret-key" "The secret key for calling the stripe api" .= monetisationConfStripeSecretKey
        <*> optionalFieldOrNull "stripe-publishable-key" "The publishable key for calling the stripe api" .= monetisationConfStripePublishableKey
        <*> optionalFieldOrNull "events-fetcher" "The configuration for the stripe events fetcher" .= monetisationConfStripeEventsFetcher
        <*> optionalFieldOrNull "events-retrier" "The configuration for the stripe events fetcher" .= monetisationConfStripeEventsRetrier
        <*> optionalFieldOrNull "max-items-free" "The number of items a free user can have on the server" .= monetisationConfMaxItemsFree

data Environment = Environment
  { envConfigFile :: !(Maybe FilePath),
    envHost :: !(Maybe String),
    envPort :: !(Maybe Int),
    envDb :: !(Maybe Text),
    envLogLevel :: !(Maybe LogLevel),
    envSigningKeyFile :: !(Maybe FilePath),
    envStripePlan :: !(Maybe String),
    envStripeSecretKey :: !(Maybe String),
    envStripePublishableKey :: !(Maybe String),
    envLooperStripeEventsFetcher :: LooperEnvironment,
    envLooperStripeEventsRetrier :: LooperEnvironment,
    envMaxItemsFree :: !(Maybe Int)
  }
  deriving (Show, Eq)

data Settings = Settings
  { setHost :: !Text,
    setPort :: !Int,
    setLogLevel :: !LogLevel,
    setSigningKeyFile :: !(Path Abs File),
    setConnectionInfo :: !SqliteConnectionInfo,
    setAdmins :: ![Username],
    setFreeloaders :: ![Username],
    setMonetisationSettings :: !(Maybe MonetisationSettings)
  }
  deriving (Show)

data MonetisationSettings = MonetisationSettings
  { monetisationSetStripeSettings :: !StripeSettings,
    monetisationSetStripeEventsFetcher :: LooperSettings,
    monetisationSetStripeEventsRetrier :: LooperSettings,
    monetisationSetMaxItemsFree :: !Int
  }
  deriving (Show)

data StripeSettings = StripeSettings
  { stripeSetPlan :: !Stripe.PlanId,
    stripeSetStripeConfig :: Stripe.StripeConfig,
    stripeSetPublishableKey :: Text
  }
  deriving (Show)
