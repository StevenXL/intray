{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Intray.Cli.NoSyncSpec
  ( spec,
  )
where

import Intray.Cli
import Intray.Cli.Commands
import Intray.Cli.OptParse
import Intray.Cli.TestUtils
import Intray.Data
import TestImport

spec :: Spec
spec = sequential . offlineCliMSpec $ do
  it "correctly errors when a user tries to register but has no server configured" $ \env ->
    testCliM
      env
      ( dispatch $
          DispatchRegister $
            RegisterSettings
              { registerSetUsername = parseUsername "testuser",
                registerSetPassword = Just "password"
              }
      )
      `shouldThrow` (\(_ :: ExitCode) -> True)

  it "correctly errors when a user tries to login but has no server configured" $ \env ->
    testCliM
      env
      ( dispatch $
          DispatchLogin $
            LoginSettings
              { loginSetUsername = parseUsername "testuser",
                loginSetPassword = Just "password"
              }
      )
      `shouldThrow` (\(_ :: ExitCode) -> True)

  it "Works fine without a server" $ \env -> testCliM env $ do
    dispatch $
      DispatchAddItem $
        AddSettings
          { addSetContents = ["hello", "world"],
            addSetReadStdin = False,
            addSetRemote = False
          }
    dispatch DispatchShowItem
    dispatch DispatchDoneItem
    dispatch DispatchSize

  specify "login fails immediately if no server is configured" $ \env ->
    testCliM
      env
      ( dispatch $
          DispatchLogin
            LoginSettings
              { loginSetUsername = Nothing,
                loginSetPassword = Nothing
              }
      )
      `shouldThrow` (== ExitFailure 1)
