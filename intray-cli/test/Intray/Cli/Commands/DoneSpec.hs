{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Intray.Cli.Commands.DoneSpec (spec) where

import qualified Data.List.NonEmpty as NE
import Intray.API.Gen ()
import Intray.Cli.OptParse
import Intray.Cli.Sqlite
import Intray.Cli.TestUtils
import Intray.Data
import TestImport

spec :: Spec
spec = do
  cliMSpec $ do
    it "can always done a local item" $ \settings ->
      forAllValid $ \parts -> do
        let intray = testIntray settings
        intray $
          DispatchAddItem
            AddSettings
              { addSetContents = NE.toList parts,
                addSetReadStdin = False,
                addSetRemote = False
              }
        intray DispatchShowItem
        intray DispatchDoneItem
        s <- testCliM settings getStoreSize
        s `shouldBe` 0
    it "can always done a local item, even with an explicit sync inbetween" $ \settings ->
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
                addSetRemote = False
              }
        intray DispatchSync
        intray DispatchShowItem
        intray DispatchDoneItem
        s <- testCliM settings getStoreSize
        s `shouldBe` 0
    it "can always done a local item, even with an explicit sync inbetween, even after syncing again" $ \settings ->
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
                addSetRemote = False
              }
        intray DispatchSync
        intray DispatchShowItem
        intray DispatchDoneItem
        intray DispatchSync
        s <- testCliM settings getStoreSize
        s `shouldBe` 0
    it "can always done a remote item" $ \settings ->
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
          testIntray settings $
            DispatchAddItem
              AddSettings
                { addSetContents = NE.toList parts,
                  addSetReadStdin = False,
                  addSetRemote = True
                }
          intray DispatchSync
          intray DispatchShowItem
          intray DispatchDoneItem
          s <- testCliM settings getStoreSize
          s `shouldBe` 0
