module Intray.Cli.Sqlite (withDB) where

import qualified Data.Text as T
import Database.Persist.Sqlite
import Import
import Intray.Cli.DB
import Intray.Cli.OptParse
import Intray.Cli.Path

withDB :: SqlPersistT CliM a -> CliM a
withDB func = do
  dbFile <- getDBPath
  withSqlitePoolInfo (mkSqliteConnectionInfo $ T.pack $ fromAbsFile dbFile) 1 $ \pool ->
    flip runSqlPool pool $ do
      _ <- runMigrationQuiet clientAutoMigration
      func
