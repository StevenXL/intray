{-# LANGUAGE OverloadedStrings #-}

module Intray.Cli.SyncSpec
  ( spec,
  )
where

import Intray.API.Gen ()
import Intray.Cli.OptParse
import Intray.Cli.Session (loadToken)
import Intray.Cli.Sqlite
import Intray.Cli.TestUtils
import Intray.Client
import Intray.Server.TestUtils
import TestImport

spec :: Spec
spec = do
  withIntrayServer . onlineCliMSpec $
    itWithBoth "correctly deletes the local LastSeen after a sync if the item has dissappeared remotely" $ \man settings ->
      forAllValid $ \ti ->
        case setBaseUrl settings of
          Nothing -> expectationFailure "Should not happen."
          Just burl -> do
            let cenv = mkClientEnv man burl
            withValidNewUserAndData cenv $ \un pw _ -> do
              let intray = testIntray settings
              intray $ DispatchLogin LoginSettings {loginSetUsername = Just un, loginSetPassword = Just pw}
              mToken <- testCliM settings loadToken
              token <-
                case mToken of
                  Nothing -> expectationFailure "Should have a token after logging in"
                  Just t -> pure t

              uuid <- runClientOrError cenv $ clientPostAddItem token ti
              intray DispatchSync
              intray DispatchShowItem
              shownItemIdBefore <- testCliM settings getShownItem
              shownItemIdBefore `shouldSatisfy` isJust
              NoContent <- runClientOrError cenv $ clientDeleteItem token uuid
              intray DispatchSync
              shownItemIdAfter <- testCliM settings getShownItem
              shownItemIdAfter `shouldSatisfy` isNothing

  let maxFree = 2
  withPaidIntrayServer maxFree . onlineCliMSpec $
    itWithBoth "Can add items past the maximum allowed number of free items locally" $ \man settings ->
      case setBaseUrl settings of
        Nothing -> expectationFailure "Should not happen."
        Just burl -> do
          let cenv = mkClientEnv man burl
          withValidNewUserAndData cenv $ \un pw _ -> do
            let intray = testIntray settings
            intray $ DispatchLogin LoginSettings {loginSetUsername = Just un, loginSetPassword = Just pw}
            let sizeShouldBe s = do
                  s' <- testCliM settings getStoreSize
                  s' `shouldBe` s
            let add ts = intray $ DispatchAddItem AddSettings {addSetContents = ts, addSetReadStdin = False, addSetRemote = False}
            sizeShouldBe 0
            add ["one"]
            sizeShouldBe 1
            intray DispatchSize
            sizeShouldBe 1
            add ["two"]
            sizeShouldBe 2
            intray DispatchSize
            sizeShouldBe 2
            add ["three"]
            sizeShouldBe 3
            intray DispatchSize
            sizeShouldBe 3
