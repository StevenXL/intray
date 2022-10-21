{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecordWildCards #-}

module Intray.Cli.TestUtils where

import Intray.Cli
import Intray.Cli.Env
import Intray.Cli.OptParse.Types
import Intray.Server.TestUtils
import qualified Network.HTTP.Client as HTTP
import System.FileLock
import Test.Syd
import Test.Syd.Path
import Test.Syd.Wai
import TestImport

intrayWithEnv :: [(String, String)] -> [String] -> IO ()
intrayWithEnv envVars args = do
  mapM_ (uncurry setEnv) envVars
  putStrLn $ unwords $ "RUNNING:" : "intray" : args
  withArgs args intrayCli

type CliSpec = TestDef '[HTTP.Manager] Settings

testIntray :: Settings -> Dispatch -> IO ()
testIntray settings d = dispatch (Instructions d settings)

testCliM :: Settings -> CliM a -> IO a
testCliM settings = runCliM settings Shared Shared -- Most likely to go wrong

cliMSpec :: CliSpec -> Spec
cliMSpec s = managerSpec $ do
  describe "online, with autosync" . modifyMaxSuccess (`div` 10) $ setupAroundWith' (\man _ -> onlineWithAutosyncEnvSetupFunc man) s
  describe "online, without autosync" . modifyMaxSuccess (`div` 10) $ setupAroundWith' (\man _ -> onlineWithoutAutosyncEnvSetupFunc man) s
  describe "offline" . modifyMaxSuccess (`div` 5) $ setupAround offlineEnvSetupFunc s

onlineCliMSpec :: CliSpec -> TestDef '[HTTP.Manager] ClientEnv
onlineCliMSpec spec = do
  describe "online, with autosync" . modifyMaxSuccess (`div` 10) $
    setupAroundWith
      (\cenv -> (\s -> s {setBaseUrl = Just (baseUrl cenv), setSyncStrategy = AlwaysSync}) <$> offlineEnvSetupFunc)
      spec
  describe "online, without autosync" . modifyMaxSuccess (`div` 10) $
    setupAroundWith
      (\cenv -> (\s -> s {setBaseUrl = Just (baseUrl cenv), setSyncStrategy = NeverSync}) <$> offlineEnvSetupFunc)
      spec

offlineCliMSpec :: CliSpec -> Spec
offlineCliMSpec = managerSpec . setupAround offlineEnvSetupFunc

offlineEnvSetupFunc :: SetupFunc Settings
offlineEnvSetupFunc = do
  tempDir <- tempDirSetupFunc "intray-cli-test-dir"
  setCacheDir <- resolveDir tempDir "cache"
  setDataDir <- resolveDir tempDir "data"
  let setSyncStrategy = NeverSync
  let setAutoOpen = DontAutoOpen
  let setLogLevel = LevelError
  let setBaseUrl = Nothing
  pure Settings {..}

onlineWithoutAutosyncEnvSetupFunc :: HTTP.Manager -> SetupFunc Settings
onlineWithoutAutosyncEnvSetupFunc man = do
  cenv <- intrayTestClientEnvSetupFunc Nothing man
  settings <- offlineEnvSetupFunc
  pure $
    settings
      { setSyncStrategy = NeverSync,
        setBaseUrl = Just (baseUrl cenv)
      }

onlineWithAutosyncEnvSetupFunc :: HTTP.Manager -> SetupFunc Settings
onlineWithAutosyncEnvSetupFunc man = do
  settings <- onlineWithoutAutosyncEnvSetupFunc man
  pure $ settings {setSyncStrategy = AlwaysSync}
