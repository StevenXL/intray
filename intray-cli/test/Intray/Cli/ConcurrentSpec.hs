{-# LANGUAGE OverloadedStrings #-}

module Intray.Cli.ConcurrentSpec
  ( spec,
  )
where

import Control.Monad.Logger
import Control.Monad.Reader
import qualified Data.Text as T
import Intray.Cli.Commands.Add
import Intray.Cli.Env
import Intray.Cli.OptParse
import Intray.Cli.Sqlite
import Intray.Cli.Store
import Intray.Cli.TestUtils
import Intray.Server.TestUtils
import TestImport
import UnliftIO

spec :: Spec
spec = sequential . cliMSpec $
  it "Going through the usual manual steps just works, even if multiple clients do it at the same time in the same place" $ \env -> testCliM env $ do
    dataDir <- asks envDataDir
    cacheDir <- asks envCacheDir
    mClientEnv <- asks envClientEnv
    let envVars =
          concat
            [ [ ("INTRAY_USERNAME", "testuser"),
                ("INTRAY_PASSWORD", "testpassword"),
                ("INTRAY_CACHE_DIR", fromAbsDir cacheDir),
                ("INTRAY_DATA_DIR", fromAbsDir dataDir)
              ],
              [ ("INTRAY_URL", showBaseUrl (baseUrl clientEnv))
                | clientEnv <- maybeToList mClientEnv
              ]
            ]

    let intray = liftIO . intrayWithEnv envVars
    intray ["register"]
    intray ["login"]
    let additions = 6
    forConcurrently_ [1 .. additions] $ \i -> do
      -- We cannot use 'intray' here because that uses 'withArgs' which is not threadsafe and causes segfaults
      addItem
        AddSettings
          { addSetContents = ["hello", "world", T.pack (show i)],
            addSetReadStdin = False,
            addSetRemote = False
          }
    s <- getStoreSize
    liftIO $ s `shouldBe` additions
