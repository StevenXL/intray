{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Intray.Server.Handler.SyncSpec
  ( spec,
  )
where

import qualified Data.Map as M
import Database.Persist.Sql
import Intray.API.Gen ()
import Intray.Client
import Intray.Server.TestUtils
import TestImport

spec :: Spec
spec =
  describe "PostSync" $ do
    withIntrayServer $ do
      it "fails without PermitSync" $ \cenv ->
        forAllValid $ \syncRequest ->
          failsWithOutPermission cenv PermitSync $ \t -> clientPostSync t syncRequest
      it "produces a valid sync result for any sync request" $ \cenv ->
        forAllValid $ \syncRequest ->
          withValidNewUser cenv $ \token -> do
            sr <- runClientOrError cenv $ clientPostSync token syncRequest
            shouldBeValid sr
      it "is idempotent" $ \cenv ->
        forAllValid $ \initStore ->
          withValidNewUser cenv $ \token -> do
            sr1 <- runClientOrError cenv $ clientPostSync token $ makeSyncRequest initStore
            let firstStore = mergeSyncResponse initStore sr1
            sr2 <- runClientOrError cenv $ clientPostSync token $ makeSyncRequest firstStore
            let secondStore = mergeSyncResponse firstStore sr2
            secondStore `shouldBe` firstStore
    let maxFree = 2
    withPaidIntrayServer maxFree $
      it "syncs at most two items if only two items are free" $
        \cenv ->
          forAllValid $ \(i1, i2, i3) ->
            withValidNewUser cenv $ \token -> do
              let store =
                    emptyClientStore
                      { clientStoreAdded =
                          M.fromList
                            [ (toSqlKey 1, i1),
                              (toSqlKey 2, i2),
                              (toSqlKey 3, i3)
                            ]
                      }
              M.size (clientStoreAdded store) `shouldBe` 3
              M.size (clientStoreSynced store) `shouldBe` 0
              sr1 <- runClientOrError cenv $ clientPostSync token $ makeSyncRequest store
              let store' = mergeSyncResponse store sr1
              M.size (clientStoreAdded store') `shouldBe` (3 - maxFree)
              M.size (clientStoreSynced store') `shouldBe` maxFree
