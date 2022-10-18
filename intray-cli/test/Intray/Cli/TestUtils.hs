{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecordWildCards #-}

module Intray.Cli.TestUtils where

import Intray.Cli (intrayCli)
import Intray.Cli.DB
import Intray.Cli.Env
import Intray.Cli.OptParse.Types (AutoOpen (..), SyncStrategy (..))
import Intray.Server.TestUtils
import qualified Network.HTTP.Client as HTTP
import qualified Network.HTTP.Types as HTTP
import Test.Syd
import Test.Syd.Path
import Test.Syd.Persistent.Sqlite
import Test.Syd.Wai
import TestImport

intrayWithEnv :: [(String, String)] -> [String] -> IO ()
intrayWithEnv envVars args = do
  mapM_ (uncurry setEnv) envVars
  putStrLn $ unwords $ "RUNNING:" : "intray" : args
  withArgs args intrayCli

type CliSpec = TestDef '[HTTP.Manager] Env

testCliM :: Env -> CliM a -> IO a
testCliM env func = do
  dontLog <- runNoLoggingT askLoggerIO
  runLoggingT (runReaderT func env) dontLog

cliMSpec :: CliSpec -> Spec
cliMSpec s = managerSpec $ do
  describe "online, with autosync" $ setupAroundWith' (\man _ -> onlineWithAutosyncEnvSetupFunc man) s
  describe "online, without autosync" $ setupAroundWith' (\man _ -> onlineWithoutAutosyncEnvSetupFunc man) s
  describe "offline" $ setupAround offlineEnvSetupFunc s

onlineCliMSpec :: CliSpec -> TestDef '[HTTP.Manager] ClientEnv
onlineCliMSpec s = do
  describe "online, with autosync" $
    setupAroundWith
      (\cenv -> (\e -> e {envClientEnv = Just cenv, envSyncStrategy = AlwaysSync}) <$> offlineEnvSetupFunc)
      s
  describe "online, without autosync" $
    setupAroundWith
      (\cenv -> (\e -> e {envClientEnv = Just cenv, envSyncStrategy = NeverSync}) <$> offlineEnvSetupFunc)
      s

offlineCliMSpec :: CliSpec -> Spec
offlineCliMSpec = managerSpec . setupAround offlineEnvSetupFunc

offlineEnvSetupFunc :: SetupFunc Env
offlineEnvSetupFunc = do
  let envBaseUrl = Nothing
  envCacheDir <- tempDirSetupFunc "intray-cli-test-cache-dir"
  envDataDir <- tempDirSetupFunc "intray-cli-test-data-dir"
  envConnectionPool <- connectionPoolSetupFunc clientAutoMigration
  let envSyncStrategy = NeverSync
  let envAutoOpen = DontAutoOpen
  let envClientEnv = Nothing
  pure Env {..}

onlineWithoutAutosyncEnvSetupFunc :: HTTP.Manager -> SetupFunc Env
onlineWithoutAutosyncEnvSetupFunc man = do
  cenv <- intrayTestClientEnvSetupFunc Nothing man
  env <- offlineEnvSetupFunc
  pure $
    env
      { envSyncStrategy = NeverSync,
        envClientEnv = Just cenv
      }

onlineWithAutosyncEnvSetupFunc :: HTTP.Manager -> SetupFunc Env
onlineWithAutosyncEnvSetupFunc man = do
  env <- onlineWithoutAutosyncEnvSetupFunc man
  pure $ env {envSyncStrategy = AlwaysSync}
