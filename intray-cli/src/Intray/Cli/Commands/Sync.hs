{-# LANGUAGE RecordWildCards #-}

module Intray.Cli.Commands.Sync (sync) where

import Import
import Intray.Cli.Client
import Intray.Cli.OptParse
import Intray.Cli.Session
import Intray.Cli.Store
import Intray.Cli.Sync
import Intray.Client

sync :: CliM ()
sync =
  withClientStore $ \before -> do
    let req = makeSyncRequest before
    mErrOrClientStore <- withToken $ \t -> runSingleClient $ clientPostSync t req
    case mErrOrClientStore of
      Nothing -> liftIO $ die "No server configured."
      Just errOrClientStore ->
        case errOrClientStore of
          Left err -> liftIO $ die $ unlines ["Sync failed:", show err]
          Right resp -> do
            liftIO $ putStr $ showMergeStats resp
            let after = mergeSyncResponse before resp
            anyUnsyncedWarning after
            pure after

showMergeStats :: SyncResponse ci si a -> String
showMergeStats SyncResponse {..} =
  unlines
    [ unwords [show $ length syncResponseServerAdded, "added   remotely"],
      unwords [show $ length syncResponseServerDeleted, "deleted remotely"],
      unwords [show $ length syncResponseClientAdded, "added   locally"],
      unwords [show $ length syncResponseClientDeleted, "deleted locally"]
    ]
