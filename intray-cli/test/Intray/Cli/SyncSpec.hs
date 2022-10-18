{-# LANGUAGE OverloadedStrings #-}

module Intray.Cli.SyncSpec
  ( spec,
  )
where

import qualified Data.Text as T
import Intray.API.Gen ()
import Intray.Cli
import Intray.Cli.Env
import Intray.Cli.OptParse
import Intray.Cli.Session (loadToken)
import Intray.Cli.Sqlite
import Intray.Cli.Store
import Intray.Cli.TestUtils
import Intray.Client
import Intray.Server.TestUtils
import TestImport

spec :: Spec
spec = do
  withIntrayServer . onlineCliMSpec $
    it "correctly deletes the local LastSeen after a sync if the item has dissappeared remotely" $ \env ->
      forAllValid $ \ti ->
        case envClientEnv env of
          Nothing -> expectationFailure "Should not happen."
          Just cenv -> withValidNewUserAndData cenv $ \un pw _ -> testCliM env $ do
            dispatch $ DispatchLogin LoginSettings {loginSetUsername = Just un, loginSetPassword = Just pw}
            mToken <- loadToken
            token <-
              case mToken of
                Nothing -> liftIO $ expectationFailure "Should have a token after logging in"
                Just t -> pure t

            uuid <- liftIO $ runClientOrError cenv $ clientPostAddItem token ti
            dispatch DispatchSync
            dispatch DispatchShowItem
            shownItemIdBefore <- getShownItem
            liftIO $ shownItemIdBefore `shouldSatisfy` isJust
            NoContent <- liftIO $ runClientOrError cenv $ clientDeleteItem token uuid
            dispatch DispatchSync
            shownItemIdAfter <- getShownItem
            liftIO $ shownItemIdAfter `shouldSatisfy` isNothing

  let maxFree = 2
  withPaidIntrayServer maxFree . onlineCliMSpec $
    it "Can add items past the maximum allowed number of free items locally" $ \env ->
      case envClientEnv env of
        Nothing -> expectationFailure "Should not happen."
        Just cenv -> withValidNewUserAndData cenv $ \un pw _ -> testCliM env $ do
          dispatch $ DispatchLogin LoginSettings {loginSetUsername = Just un, loginSetPassword = Just pw}
          let sizeShouldBe s = do
                s' <- getStoreSize
                liftIO $ s' `shouldBe` s
          let add ts = dispatch $ DispatchAddItem AddSettings {addSetContents = ts, addSetReadStdin = False, addSetRemote = False}
          sizeShouldBe 0
          add ["one"]
          sizeShouldBe 1
          dispatch DispatchSize
          sizeShouldBe 1
          add ["two"]
          sizeShouldBe 2
          dispatch DispatchSize
          sizeShouldBe 2
          add ["three"]
          sizeShouldBe 3
          dispatch DispatchSize
          sizeShouldBe 3
