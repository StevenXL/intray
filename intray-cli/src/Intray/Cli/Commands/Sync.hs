{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Intray.Cli.Commands.Sync (sync) where

import Control.Monad.Logger
import qualified Data.Text as T
import Import
import Intray.Cli.Client
import Intray.Cli.Env
import Intray.Cli.OptParse
import Intray.Cli.Session
import Intray.Cli.Sqlite
import Intray.Cli.Store
import Intray.Cli.Sync
import Intray.Client (SyncResponse (..), clientPostSync)
import Servant.Client

sync :: CliM ()
sync = do
  mClientEnv <- asks envClientEnv
  case mClientEnv of
    Nothing -> logErrorN "No server configured."
    Just clientEnv -> withToken $ \token -> do
      syncRequest <- makeSyncRequest
      errOrSyncResponse <- liftIO $ runClientM (clientPostSync token syncRequest) clientEnv
      case errOrSyncResponse of
        Left err -> logErrorN $ T.pack $ unlines ["Failed to sync:", show err]
        Right syncResponse -> do
          mergeSyncResponse syncResponse
          logInfoN $ T.pack $ showSyncStats syncResponse
          runDB anyUnsyncedWarning

showSyncStats :: SyncResponse ci si a -> String
showSyncStats SyncResponse {..} =
  unlines
    [ unwords [show $ length syncResponseServerAdded, "added   remotely"],
      unwords [show $ length syncResponseServerDeleted, "deleted remotely"],
      unwords [show $ length syncResponseClientAdded, "added   locally"],
      unwords [show $ length syncResponseClientDeleted, "deleted locally"]
    ]
