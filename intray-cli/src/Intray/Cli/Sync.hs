module Intray.Cli.Sync
  ( autoSyncStore,
    anyUnsyncedWarning,
  )
where

import Control.Monad.Logger
import qualified Data.Text as T
import Database.Persist
import Database.Persist.Sql
import Import
import Intray.Cli.Client
import Intray.Cli.DB
import Intray.Cli.Env
import Intray.Cli.OptParse
import Intray.Cli.Session
import Intray.Cli.Sqlite
import Intray.Cli.Store
import Intray.Client (clientPostSync)

autoSyncStore :: CliM ()
autoSyncStore = do
  syncStrategy <- asks envSyncStrategy
  case syncStrategy of
    NeverSync -> pure ()
    AlwaysSync -> do
      withToken $ \token -> do
        syncRequest <- makeSyncRequest
        mSyncResponse <- runSingleClientOrErr (clientPostSync token (undefined syncRequest))
        forM_ mSyncResponse $ \syncResponse -> mergeSyncResponse (undefined syncResponse)
      runDB anyUnsyncedWarning

anyUnsyncedWarning :: (MonadIO m, MonadLogger m) => SqlPersistT m ()
anyUnsyncedWarning = do
  mUnsynced <- selectFirst [ClientItemServerIdentifier ==. Nothing] []
  case mUnsynced of
    Nothing -> pure ()
    Just _ ->
      logWarnN $
        T.pack $
          unlines
            [ "Not all added items were synchronized in the most recent synchronisation.",
              "This may have occurred if you have not subscribed with your sync server.",
              "If that is the case, please navigate to your sync server's web interface to subscribe."
            ]
