{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}

module Intray.Server.Handler.Stripe
  ( PaidStatus (..),
    getUserPaidStatus,
  )
where

import Data.Time
import Database.Persist
import Import
import Intray.API
import Intray.Server.Handler.Utils
import Intray.Server.Types
import Servant
import Servant.Auth.Server

getUserPaidStatus :: AccountUUID -> IntrayHandler PaidStatus
getUserPaidStatus userId = do
  mss <- asks envMonetisation
  case mss of
    Nothing -> pure NoPaymentNecessary
    Just MonetisationSettings {..} -> do
      mu <- runDB $ getBy $ UniqueUserIdentifier userId
      case mu of
        Nothing -> throwAll err404
        Just (Entity _ User {..}) -> do
          isAdmin <- asks ((userUsername `elem`) . envAdmins)
          if isAdmin
            then pure NoPaymentNecessary
            else do
              isFreeloader <- asks ((userUsername `elem`) . envFreeloaders)
              if isFreeloader
                then pure NoPaymentNecessary
                else do
                  mSub <- hasSubscribed userId
                  case mSub of
                    Just u -> pure $ HasPaid u
                    Nothing -> do
                      c <- runDB $ count [IntrayItemUserId ==. userId]
                      pure $ HasNotPaid (monetisationSetMaxItemsFree - c)

hasSubscribed :: AccountUUID -> IntrayHandler (Maybe UTCTime)
hasSubscribed uuid = runDB $ fmap (fmap (subscriptionEnd . entityVal)) $ getBy $ UniqueSubscriptionUser uuid
