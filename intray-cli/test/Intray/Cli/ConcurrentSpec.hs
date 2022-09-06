{-# LANGUAGE OverloadedStrings #-}

module Intray.Cli.ConcurrentSpec
  ( spec,
  )
where

import Control.Concurrent.Async
import Control.Monad.Reader
import qualified Data.Text as T
import Intray.Cli.Commands.Add
import Intray.Cli.OptParse
import Intray.Cli.Store
import Intray.Cli.TestUtils
import Intray.Server.TestUtils
import TestImport

spec :: Spec
spec = sequential $
  withIntrayServer $
    it "Going through the usual manual steps just works, even if multiple clients do it at the same time in the same place" $ \cenv ->
      withSystemTempDir "intray-cli-test-cache" $ \cacheDir ->
        withSystemTempDir "intray-cli-test-data" $ \dataDir -> do
          let burl = baseUrl cenv
          let envVars =
                [ ("INTRAY_USERNAME", "testuser"),
                  ("INTRAY_PASSWORD", "testpassword"),
                  ("INTRAY_URL", showBaseUrl burl),
                  ("INTRAY_CACHE_DIR", fromAbsDir cacheDir),
                  ("INTRAY_DATA_DIR", fromAbsDir dataDir)
                ]

          let intray = intrayWithEnv envVars
          intray ["register"]
          intray ["login"]
          let additions = 6
          forConcurrently_ [1 .. additions] $ \i ->
            -- We cannot use 'intray' here because that uses 'withArgs' which is not threadsafe and causes segfaults
            do
              let sets =
                    Settings
                      { setBaseUrl = Just burl,
                        setCacheDir = cacheDir,
                        setDataDir = dataDir,
                        setSyncStrategy = AlwaysSync,
                        setAutoOpen = DontAutoOpen
                      }
              flip runReaderT sets $
                addItem
                  AddSettings
                    { addSetContents = ["hello", "world", T.pack (show i)],
                      addSetReadStdin = False,
                      addSetRemote = False
                    }
          let sets =
                Settings
                  { setBaseUrl = Just burl,
                    setCacheDir = cacheDir,
                    setDataDir = dataDir,
                    setSyncStrategy = NeverSync,
                    setAutoOpen = DontAutoOpen
                  }
          s <- runReaderT readClientStoreSize sets
          s `shouldBe` additions
