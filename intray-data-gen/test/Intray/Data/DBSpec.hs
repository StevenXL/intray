module Intray.Data.DBSpec (spec) where

import Intray.Data.DB
import Test.Syd
import Test.Syd.Persistent.Sqlite

spec :: Spec
spec = do
  sqliteMigrationSucceedsSpec "test_resources/migration.sql" serverAutoMigration
