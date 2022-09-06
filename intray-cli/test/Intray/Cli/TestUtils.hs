module Intray.Cli.TestUtils (intrayWithEnv) where

import Intray.Cli (intrayCli)
import TestImport

intrayWithEnv :: [(String, String)] -> [String] -> IO ()
intrayWithEnv envVars args = do
  mapM_ (uncurry setEnv) envVars
  putStrLn $ unwords $ "RUNNING:" : "intray" : args
  withArgs args intrayCli
