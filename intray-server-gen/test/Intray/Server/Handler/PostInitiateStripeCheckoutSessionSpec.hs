{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Intray.Server.Handler.PostInitiateStripeCheckoutSessionSpec (spec) where

import Intray.Client
import Intray.Server.Handler.PostInitiateStripeCheckoutSession
import Test.Syd.Aeson
import TestImport

spec :: Spec
spec = do
  it "tries to make the same PostCustomersRequestBody as before" $
    goldenJSONValueFile "test_resources/stripe/post-customer.json" $ do
      username <- parseUsername "username"
      pure $ mkPostCustomersRequestBodyForUser username
  it "tries to make the same PostCheckoutSessionsRequestBody as before" $
    goldenJSONValueFile "test_resources/stripe/post-checkout-session.json" $ do
      let initiateStripeCheckoutSession =
            InitiateStripeCheckoutSession
              { initiateStripeCheckoutSessionSuccessUrl = "http://intray.example.com/stripe/success",
                initiateStripeCheckoutSessionCanceledUrl = "http://intray.example.com/stripe/cancel"
              }
      username <- parseUsername "username"
      let customerId = "customer-id"
      let planId = "plan-id"
      pure $ mkPostCheckoutSessionsRequestBodyForUser initiateStripeCheckoutSession username customerId planId
