{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Intray.Server.Handler.PostInitiateStripeCheckoutSession
  ( servePostInitiateStripeCheckoutSession,
  )
where

import Data.Aeson as JSON
import Data.Aeson.Encode.Pretty as JSON
import qualified Data.ByteString.Lazy as LB
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Version
import Database.Persist
import Import
import Intray.API
import Intray.Server.Handler.Utils
import Intray.Server.OptParse.Types
import Intray.Server.Types
import Network.HTTP.Client as HTTP
import Paths_intray_server
import Servant
import StripeClient as Stripe

servePostInitiateStripeCheckoutSession ::
  AuthCookie ->
  InitiateStripeCheckoutSession ->
  IntrayHandler InitiatedCheckoutSession
servePostInitiateStripeCheckoutSession AuthCookie {..} iscs = do
  mMonetisationEnv <- asks envMonetisation
  case mMonetisationEnv of
    Nothing -> throwError err404
    Just MonetisationEnv {..} -> do
      let StripeSettings {..} = monetisationEnvStripeSettings
      let config =
            Stripe.defaultConfiguration
              { configSecurityScheme = bearerAuthenticationSecurityScheme stripeSetSecretKey
              }
      mAccount <- runDB $ getBy $ UniqueUserIdentifier authCookieUserUUID
      case mAccount of
        Nothing -> throwError err404 {errBody = "User not found."}
        Just (Entity _ User {..}) -> do
          -- Get or create the stripe customer
          mStripeCustomer <- runDB $ selectFirst [StripeCustomerUser ==. authCookieUserUUID] [Desc StripeCustomerId]
          let metadata =
                HM.fromList
                  [ ("product", "intray"),
                    ("intray-server-version", toJSON $ showVersion version)
                  ]
          Entity _ stripeCustomer <- case mStripeCustomer of
            Just sce -> pure sce
            Nothing -> do
              let postCustomersRequest =
                    mkPostCustomersRequestBody
                      { postCustomersRequestBodyDescription = Just $ usernameText userUsername,
                        postCustomersRequestBodyMetadata =
                          Just $
                            PostCustomersRequestBodyMetadata'Object metadata
                      }
              resp <- liftIO $ runWithConfiguration config $ postCustomers $ Just postCustomersRequest
              case responseBody resp of
                PostCustomersResponseError err -> throwError err500 {errBody = LB.fromStrict $ TE.encodeUtf8 $ "Something went wrong while parsing stripe's response:\n" <> T.pack err}
                PostCustomersResponseDefault err -> throwError err500 {errBody = "Error while calling stripe:\n" <> JSON.encodePretty err}
                PostCustomersResponse200 Customer {..} ->
                  -- Keep track of it in our database for later
                  runDB $
                    upsertBy
                      (UniqueStripeCustomer authCookieUserUUID customerId)
                      (StripeCustomer {stripeCustomerUser = authCookieUserUUID, stripeCustomerCustomer = customerId})
                      [StripeCustomerCustomer =. customerId]

          -- Make the request to create a checkout session
          let successUrl = initiateStripeCheckoutSessionSuccessUrl iscs
              cancelUrl = initiateStripeCheckoutSessionCanceledUrl iscs
          let request =
                (mkPostCheckoutSessionsRequestBody cancelUrl successUrl)
                  { postCheckoutSessionsRequestBodyCustomer = Just $ stripeCustomerCustomer stripeCustomer,
                    postCheckoutSessionsRequestBodyClientReferenceId = Just $ usernameText userUsername,
                    postCheckoutSessionsRequestBodyLineItems = Nothing,
                    postCheckoutSessionsRequestBodyMode = Just PostCheckoutSessionsRequestBodyMode'EnumSubscription,
                    postCheckoutSessionsRequestBodyMetadata = Just metadata,
                    postCheckoutSessionsRequestBodySubscriptionData =
                      Just $
                        mkPostCheckoutSessionsRequestBodySubscriptionData'
                          { postCheckoutSessionsRequestBodySubscriptionData'Metadata = Just metadata,
                            postCheckoutSessionsRequestBodySubscriptionData'Items =
                              Just
                                [ mkPostCheckoutSessionsRequestBodySubscriptionData'Items' stripeSetPlan
                                ]
                          }
                  }

          -- Actually perform the request
          resp <- liftIO $ runWithConfiguration config $ postCheckoutSessions request :: IntrayHandler (HTTP.Response PostCheckoutSessionsResponse)
          case responseBody resp of
            PostCheckoutSessionsResponseError err -> throwError err500 {errBody = LB.fromStrict $ TE.encodeUtf8 $ "Something went wrong while parsing stripe's response:" <> T.pack err}
            PostCheckoutSessionsResponseDefault err -> throwError err500 {errBody = "Error while calling stripe:\n" <> JSON.encodePretty err}
            PostCheckoutSessionsResponse200 session -> do
              pure $
                InitiatedCheckoutSession
                  { initiatedCheckoutSessionId = checkout'sessionId session,
                    initiatedCheckoutSessionCustomerId = case checkout'sessionCustomer session of
                      Nothing -> Nothing
                      Just (Checkout'sessionCustomer'Text cid) -> Just cid
                      Just (Checkout'sessionCustomer'Customer c) -> Just (customerId c)
                      Just (Checkout'sessionCustomer'DeletedCustomer _) -> Nothing
                  }
