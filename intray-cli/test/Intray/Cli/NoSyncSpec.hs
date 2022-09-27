{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Intray.Cli.NoSyncSpec
  ( spec,
  )
where

import Intray.Cli
import Intray.Cli.OptParse
import Intray.Data
import TestImport

spec :: Spec
spec = sequential $ do
  it "correctly errors when a user tries to register but has no server configured" $
    withSystemTempDir "intray-cli-test-data" $ \dataDir ->
      withSystemTempDir "intray-cli-test-cache" $ \cacheDir -> do
        let sets =
              Settings
                { setBaseUrl = Nothing,
                  setCacheDir = cacheDir,
                  setDataDir = dataDir,
                  setSyncStrategy = NeverSync,
                  setAutoOpen = DontAutoOpen
                }
        dontLog <- runNoLoggingT askLoggerIO
        let intray d = runLoggingT (runReaderT (dispatch d) sets) dontLog
        let rs =
              RegisterSettings
                { registerSetUsername = parseUsername "testuser",
                  registerSetPassword = Just "password"
                }
        intray (DispatchRegister rs) `shouldThrow` (\(_ :: ExitCode) -> True)
  it "correctly errors when a user tries to login but has no server configured" $
    withSystemTempDir "intray-cli-test-data" $ \dataDir ->
      withSystemTempDir "intray-cli-test-cache" $ \cacheDir -> do
        let sets =
              Settings
                { setBaseUrl = Nothing,
                  setCacheDir = cacheDir,
                  setDataDir = dataDir,
                  setSyncStrategy = NeverSync,
                  setAutoOpen = DontAutoOpen
                }
        dontLog <- runNoLoggingT askLoggerIO
        let intray d = runLoggingT (runReaderT (dispatch d) sets) dontLog
        let rs =
              LoginSettings
                { loginSetUsername = parseUsername "testuser",
                  loginSetPassword = Just "password"
                }
        intray (DispatchLogin rs) `shouldThrow` (\(_ :: ExitCode) -> True)
  it "Works fine without a server" $
    withSystemTempDir "intray-cli-test-cache" $ \cacheDir ->
      withSystemTempDir "intray-cli-test-data" $ \dataDir -> do
        let sets =
              Settings
                { setBaseUrl = Nothing,
                  setCacheDir = cacheDir,
                  setDataDir = dataDir,
                  setSyncStrategy = NeverSync,
                  setAutoOpen = DontAutoOpen
                }
        dontLog <- runNoLoggingT askLoggerIO
        let intray d = runLoggingT (runReaderT (dispatch d) sets) dontLog
        intray $
          DispatchAddItem $
            AddSettings
              { addSetContents = ["hello", "world"],
                addSetReadStdin = False,
                addSetRemote = False
              }
        intray DispatchShowItem
        intray DispatchDoneItem
        intray DispatchSize
  specify "login fails immediately if no server is configured" $
    withSystemTempDir "intray-cli-test-cache" $ \cacheDir ->
      withSystemTempDir "intray-cli-test-data" $ \dataDir -> do
        let sets =
              Settings
                { setBaseUrl = Nothing,
                  setCacheDir = cacheDir,
                  setDataDir = dataDir,
                  setSyncStrategy = NeverSync,
                  setAutoOpen = DontAutoOpen
                }
        dontLog <- runNoLoggingT askLoggerIO
        let intray d = runLoggingT (runReaderT (dispatch d) sets) dontLog
        intray
          (DispatchLogin LoginSettings {loginSetUsername = Nothing, loginSetPassword = Nothing})
          `shouldThrow` (== ExitFailure 1)
