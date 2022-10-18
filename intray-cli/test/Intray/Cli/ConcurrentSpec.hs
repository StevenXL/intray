{-# LANGUAGE OverloadedStrings #-}

module Intray.Cli.ConcurrentSpec
  ( spec,
  )
where

import Control.Monad.Reader
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
  xdescribe "This test fails because the locking happens on an upper layer" $
    it "Going through the usual manual steps just works, even if multiple clients do it at the same time in the same place" $ \env -> testCliM env $ do
      dispatch $
        DispatchRegister
          RegisterSettings
            { registerSetUsername = parseUsername "testuser",
              registerSetPassword = Just "testpassword"
            }
      dispatch $
        DispatchLogin
          LoginSettings
            { loginSetUsername = parseUsername "testuser",
              loginSetPassword = Just "testpassword"
            }

      let additions = 6
      forConcurrently_ [1 .. additions] $ \i -> do
        dispatch $
          DispatchAddItem
            AddSettings
              { addSetContents = ["hello", "world", T.pack (show i)],
                addSetReadStdin = False,
                addSetRemote = False
              }
      s <- getStoreSize
      liftIO $ s `shouldBe` additions
