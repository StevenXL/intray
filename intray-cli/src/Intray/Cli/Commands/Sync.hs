{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Intray.Cli.Commands.Sync (sync) where

import Control.Monad.Logger
import qualified Data.Set as S
import qualified Data.Text as T
import Database.Persist
import Import
import Intray.Cli.Client
import Intray.Cli.DB
import Intray.Cli.Env
import Intray.Cli.OptParse
import Intray.Cli.Session
import Intray.Cli.Sqlite
import Intray.Cli.Store
import Intray.Cli.Sync
import Intray.Client (ItemUUID, SyncResponse (..), clientPostSync)
import Servant.Client

sync :: CliM ()
sync = do
  mClientEnv <- asks envClientEnv
  case mClientEnv of
    Nothing -> logErrorN "No server configured."
    Just clientEnv -> withToken $ \token -> do
      syncRequest <- makeSyncRequest
      mShownItemUuid <- do
        mSi <- getShownItem
        runDB $
          fmap (join . join) $
            forM mSi $ fmap (fmap clientItemServerIdentifier) . get
      errOrSyncResponse <- liftIO $ runClientM (clientPostSync token syncRequest) clientEnv
      case errOrSyncResponse of
        Left err -> logErrorN $ T.pack $ unlines ["Failed to sync:", show err]
        Right syncResponse -> do
          -- If the shown item was deleted then we have to clear it because
          -- otherwise it will refer to a row that won't exist anymore when the
          -- sync response is merged.
          let shownItemWasDeleted = case mShownItemUuid :: Maybe ItemUUID of
                Nothing -> False
                Just i ->
                  S.member i (syncResponseServerDeleted syncResponse)
                    || S.member i (syncResponseClientDeleted syncResponse)
          when shownItemWasDeleted clearShownItem
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
