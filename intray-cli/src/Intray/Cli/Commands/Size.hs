module Intray.Cli.Commands.Size (size) where

import Import
import Intray.Cli.OptParse
import Intray.Cli.Store
import Intray.Cli.Sync

size :: CliM ()
size = do
  s <- syncAndReturn storeSize
  liftIO $ print (s :: Int)
