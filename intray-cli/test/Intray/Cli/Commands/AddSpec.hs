{-# LANGUAGE OverloadedStrings #-}

module Intray.Cli.Commands.AddSpec (spec) where

import Intray.API.Gen ()
import Intray.Cli.OptParse
import Intray.Cli.Sqlite
import Intray.Cli.TestUtils
import TestImport

spec :: Spec
spec = cliMSpec $ do
  it "can always add an item locally" $ \settings ->
    forAllValid $ \parts -> do
      testIntray settings $
        DispatchAddItem
          AddSettings
            { addSetContents = parts,
              addSetReadStdin = False,
              addSetRemote = False
            }
      s <- testCliM settings getStoreSize
      s `shouldBe` 1
