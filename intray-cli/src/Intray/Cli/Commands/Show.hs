{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

module Intray.Cli.Commands.Show (showItem) where

import Data.Time
import Database.Persist
import Import
import Intray.Cli.DB
import Intray.Cli.Env
import Intray.Cli.OptParse
import Intray.Cli.Sqlite
import Intray.Cli.Store
import Intray.Cli.Sync

showItem :: CliM ()
showItem = do
  autoSyncStore
  now <- liftIO getCurrentTime
  mItem <- produceShownItem
  case mItem of
    Nothing -> liftIO $ putStrLn "Done."
    Just item -> prettyShowItemAndWait now item
