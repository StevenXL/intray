module Intray.Cli.Sync
  ( syncStore,
  )
where

import Data.Mergeless.Persistent
import Database.Persist
import Database.Persist.Sql
import Import
import Intray.Cli.Client
import Intray.Cli.DB
import Intray.Cli.OptParse
import Intray.Cli.Session
import Intray.Cli.Store
import Intray.Client

syncStore :: SqlPersistT CliM ()
syncStore = do
  syncRequest <- clientMakeSyncRequestQuery toAddedItem ClientItemServerIdentifier ClientItemDeleted
  liftIO $ print syncRequest

anyUnsyncedWarning :: SqlPersistT CliM ()
anyUnsyncedWarning = do
  mUnsynced <- selectFirst [ClientItemServerIdentifier ==. Nothing] []
  case mUnsynced of
    Nothing -> pure ()
    Just _ ->
      liftIO $
        putStrLn $
          unlines
            [ "Not all added items were synchronized in the most recent synchronisation.",
              "This may have occurred if you have not subscribed with your sync server.",
              "If that is the case, please navigate to your sync server's web interface to subscribe."
            ]
