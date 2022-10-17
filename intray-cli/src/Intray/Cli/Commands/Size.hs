module Intray.Cli.Commands.Size (size) where

import Database.Persist
import Import
import Intray.Cli.DB
import Intray.Cli.Env
import Intray.Cli.OptParse
import Intray.Cli.Sqlite
import Intray.Cli.Sync

size :: CliM ()
size = do
  autoSyncStore
  c <- getStoreSize
  liftIO $ print c
