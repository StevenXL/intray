module Intray.CliSpec
  ( spec,
  )
where

import Intray.Cli.TestUtils
import Intray.Server.TestUtils
import TestImport

spec :: Spec
spec = sequential $
  withIntrayServer $
    do
      it "Going through the usual manual steps 'just works'" $ \cenv ->
        withSystemTempDir "intray-cli-test-cache" $ \cacheDir ->
          withSystemTempDir "intray-cli-test-data" $ \dataDir -> do
            let envVars =
                  [ ("INTRAY_USERNAME", "testuser"),
                    ("INTRAY_PASSWORD", "testpassword"),
                    ("INTRAY_URL", showBaseUrl $ baseUrl cenv),
                    ("INTRAY_CACHE_DIR", fromAbsDir cacheDir),
                    ("INTRAY_DATA_DIR", fromAbsDir dataDir),
                    ("INTRAY_AUTO_OPEN", "true")
                  ]
            let intray = intrayWithEnv envVars
            intray ["register"]
            intray ["login"]
            intray ["add", "hello", "world"]
            intray ["show"]
            intray ["done"]
            intray ["size"]
            intray ["sync"]
            intray ["logout"]
      it "Going through the usual manual steps 'just works' with a nonexistent data dir" $ \cenv ->
        forAllValid $ \relDataDir ->
          withSystemTempDir "intray-cli-test-cache" $ \cacheDir ->
            withSystemTempDir "intray-cli-test-data" $ \dataDir -> do
              let envVars =
                    [ ("INTRAY_USERNAME", "testuser"),
                      ("INTRAY_PASSWORD", "testpassword"),
                      ("INTRAY_URL", showBaseUrl $ baseUrl cenv),
                      ("INTRAY_CACHE_DIR", fromAbsDir cacheDir),
                      ("INTRAY_DATA_DIR", fromAbsDir $ dataDir </> relDataDir),
                      ("INTRAY_AUTO_OPEN", "true")
                    ]
              let intray = intrayWithEnv envVars
              intray ["register"]
              intray ["login"]
              intray ["add", "hello", "world"]
              intray ["show"]
              intray ["done"]
              intray ["size"]
              intray ["sync"]
              intray ["logout"]
      it "Going through the usual manual steps 'just works' with a nonexistent cache dir" $ \cenv ->
        forAllValid $ \relCacheDir ->
          withSystemTempDir "intray-cli-test-cache" $ \cacheDir ->
            withSystemTempDir "intray-cli-test-data" $ \dataDir -> do
              let envVars =
                    [ ("INTRAY_USERNAME", "testuser"),
                      ("INTRAY_PASSWORD", "testpassword"),
                      ("INTRAY_URL", showBaseUrl $ baseUrl cenv),
                      ("INTRAY_CACHE_DIR", fromAbsDir $ cacheDir </> relCacheDir),
                      ("INTRAY_DATA_DIR", fromAbsDir dataDir),
                      ("INTRAY_AUTO_OPEN", "true")
                    ]
              let intray = intrayWithEnv envVars
              intray ["register"]
              intray ["login"]
              intray ["add", "hello", "world"]
              intray ["show"]
              intray ["done"]
              intray ["size"]
              intray ["sync"]
              intray ["logout"]
