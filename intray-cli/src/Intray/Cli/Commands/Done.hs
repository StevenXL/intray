module Intray.Cli.Commands.Done (doneItem) where

import Import
import Intray.Cli.OptParse
import Intray.Cli.Store
import Intray.Cli.Sync

doneItem :: ReaderT Settings IO ()
doneItem = do
  mii <- readLastSeen
  case mii of
    Nothing -> liftIO $ die "Are you sure?, it doesn't look like you showed an item yet."
    Just li -> do
      modifyClientStoreAndSync $ doneLastItem li
      clearLastSeen
