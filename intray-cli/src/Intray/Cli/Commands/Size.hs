module Intray.Cli.Commands.Size (size) where

import Database.Persist
import Import
import Intray.Cli.DB
import Intray.Cli.OptParse
import Intray.Cli.Sqlite

size :: CliM ()
size = withDB $ do
  c <- count ([] :: [Filter ClientItem])
  liftIO $ print c
