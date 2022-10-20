{-# LANGUAGE OverloadedStrings #-}

module Intray.Cli.ConcurrentSpec
  ( spec,
  )
where

import qualified Data.Text as T
import Intray.Cli
import Intray.Cli.OptParse
import Intray.Cli.Sqlite
import Intray.Cli.TestUtils
import Intray.Data
import TestImport
import UnliftIO

spec :: Spec
spec = sequential . cliMSpec $
  it "Going through the usual manual steps just works, even if multiple clients do it at the same time in the same place" $ \settings -> do
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

    let additions = 6
    forConcurrently_ [1 .. additions] $ \i -> do
      intray $
        DispatchAddItem
          AddSettings
            { addSetContents = ["hello", "world", T.pack (show i)],
              addSetReadStdin = False,
              addSetRemote = False
            }
    s <- testCliM settings getStoreSize
    s `shouldBe` additions
