{-# LANGUAGE OverloadedStrings #-}

module Intray.Cli.SyncSpec
  ( spec,
  )
where

import qualified Data.Text as T
import Intray.API.Gen ()
import Intray.Cli.OptParse
import Intray.Cli.Session (loadToken)
import Intray.Cli.Store
import Intray.Cli.TestUtils
import Intray.Client
import Intray.Server.TestUtils
import TestImport

spec :: Spec
spec = sequential $ do
  withIntrayServer $
    it "correctly deletes the local LastSeen after a sync if the item has dissappeared remotely" $ \cenv ->
      forAllValid $ \ti ->
        withValidNewUserAndData cenv $ \un pw _ ->
          withSystemTempDir "intray-cli-test-data" $ \dataDir ->
            withSystemTempDir "intray-cli-test-cache" $ \cacheDir -> do
              let envVars =
                    [ ("INTRAY_USERNAME", T.unpack $ usernameText un),
                      ("INTRAY_PASSWORD", T.unpack pw),
                      ("INTRAY_URL", showBaseUrl $ baseUrl cenv),
                      ("INTRAY_CACHE_DIR", fromAbsDir cacheDir),
                      ("INTRAY_DATA_DIR", fromAbsDir dataDir),
                      ("INTRAY_AUTO_OPEN", "true")
                    ]

              let intray = intrayWithEnv envVars
              intray ["login"]
              let sets =
                    Settings
                      { setBaseUrl = Just $ baseUrl cenv,
                        setCacheDir = cacheDir,
                        setDataDir = dataDir,
                        setSyncStrategy = NeverSync,
                        setAutoOpen = DontAutoOpen
                      }
              mToken <- runReaderT loadToken sets
              token <-
                case mToken of
                  Nothing -> expectationFailure "Should have a token after logging in"
                  Just t -> pure t
              uuid <- runClientOrError cenv $ clientPostAddItem token ti
              intray ["sync"]
              intray ["show"]
              mLastSeen1 <- runReaderT readLastSeen sets
              mLastSeen1 `shouldSatisfy` isJust
              NoContent <- runClientOrError cenv $ clientDeleteItem token uuid
              intray ["sync"]
              mLastSeen2 <- runReaderT readLastSeen sets
              mLastSeen2 `shouldSatisfy` isNothing
  let maxFree = 2
  withPaidIntrayServer maxFree $
    it "Can add items past the maximum allowed number of free items locally" $ \cenv ->
      withValidNewUserAndData cenv $ \un pw _ ->
        withSystemTempDir "intray-cli-test-data" $ \dataDir ->
          withSystemTempDir "intray-cli-test-cache" $ \cacheDir -> do
            let envVars =
                  [ ("INTRAY_USERNAME", T.unpack $ usernameText un),
                    ("INTRAY_PASSWORD", T.unpack pw),
                    ("INTRAY_URL", showBaseUrl $ baseUrl cenv),
                    ("INTRAY_CACHE_DIR", fromAbsDir cacheDir),
                    ("INTRAY_DATA_DIR", fromAbsDir dataDir),
                    ("INTRAY_SYNC_STRATEGY", "AlwaysSync"),
                    ("INTRAY_AUTO_OPEN", "true")
                  ]
            let intray = intrayWithEnv envVars
            intray ["login"]
            let sets =
                  Settings
                    { setBaseUrl = Just $ baseUrl cenv,
                      setCacheDir = cacheDir,
                      setDataDir = dataDir,
                      setSyncStrategy = AlwaysSync,
                      setAutoOpen = DontAutoOpen
                    }
            let size = runReaderT readClientStoreSize sets
            size `shouldReturn` 0
            intray ["add", "one"]
            size `shouldReturn` 1
            intray ["size"]
            size `shouldReturn` 1
            intray ["add", "two"]
            size `shouldReturn` 2
            intray ["size"]
            size `shouldReturn` 2
            intray ["add", "three"]
            size `shouldReturn` 3
            intray ["size"]
            size `shouldReturn` 3
