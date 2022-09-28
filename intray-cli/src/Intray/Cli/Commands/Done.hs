module Intray.Cli.Commands.Done (doneItem) where

import Database.Persist as DB
import Import
import Intray.Cli.DB
import Intray.Cli.OptParse
import Intray.Cli.Sqlite

doneItem :: CliM ()
doneItem = withDB $ do
  mClientItemId <- fmap entityKey <$> selectFirst [] [Asc ClientItemId]
  forM_ mClientItemId DB.delete
