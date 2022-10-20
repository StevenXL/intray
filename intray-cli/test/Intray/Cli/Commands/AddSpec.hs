{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Intray.Cli.Commands.AddSpec (spec) where

import qualified Data.List.NonEmpty as NE
import Intray.API.Gen ()
import Intray.Cli.OptParse
import Intray.Cli.Sqlite
import Intray.Cli.TestUtils
import Intray.Data
import Intray.Server.TestUtils
import TestImport

spec :: Spec
spec = do
  cliMSpec $
    it "can always add an item locally" $ \settings ->
      forAllValid $ \parts -> do
        testIntray settings $
          DispatchAddItem
            AddSettings
              { addSetContents = NE.toList parts,
                addSetReadStdin = False,
                addSetRemote = False
              }
        s <- testCliM settings getStoreSize
        s `shouldBe` 1
  withIntrayServer . onlineCliMSpec $
    it "can add an item remotely when there's a server" $ \settings ->
      forAllValid $ \parts -> do
        let intray = testIntray settings
        forM_ (setBaseUrl settings) $ \_ -> do
          intray $
            DispatchRegister
              RegisterSettings
                { registerSetUsername = parseUsername "testuser",
                  registerSetPassword = Just "testpassword"
                }
          intray $
            DispatchLogin
              LoginSettings
                { loginSetUsername = parseUsername "testuser",
                  loginSetPassword = Just "testpassword"
                }

        intray $
          DispatchAddItem
            AddSettings
              { addSetContents = NE.toList parts,
                addSetReadStdin = False,
                addSetRemote = True
              }
        s <- testCliM settings getStoreSize
        s `shouldBe` 0
  offlineCliMSpec $
    it "fails to add an item remotely when no server is configured" $ \settings ->
      forAllValid $ \parts -> do
        testIntray
          settings
          ( DispatchAddItem
              AddSettings
                { addSetContents = NE.toList parts,
                  addSetReadStdin = False,
                  addSetRemote = True
                }
          )
          `shouldThrow` (\(_ :: ExitCode) -> True)
