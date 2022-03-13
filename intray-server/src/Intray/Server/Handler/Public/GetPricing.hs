{-# LANGUAGE RecordWildCards #-}

module Intray.Server.Handler.Public.GetPricing
  ( serveGetPricing,
  )
where

import Import
import Intray.API
import Intray.Server.OptParse.Types
import Intray.Server.Types

serveGetPricing :: IntrayHandler (Maybe Pricing)
serveGetPricing = do
  mMone <- asks envMonetisation
  forM mMone $ \MonetisationSettings {..} -> do
    let StripeSettings {..} = monetisationSetStripeSettings
    let pricingPlan = stripeSetPlan
        pricingPrice = monetisationSetPrice
        pricingStripePublishableKey = stripeSetPublishableKey
        pricingMaxItemsFree = monetisationSetMaxItemsFree
    pure Pricing {..}
