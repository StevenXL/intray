{-# LANGUAGE OverloadedStrings #-}

module Intray.Cli.Commands.Done (doneItem) where

import Control.Monad.Logger
import Database.Persist as DB
import Import
import Intray.Cli.Env
import Intray.Cli.Sqlite

doneItem :: CliM ()
doneItem = do
  mShownItem <- getShownItem
  case mShownItem of
    Nothing -> logWarnN "Are you sure?, it doesn't look like you showed an item yet."
    Just ci -> do
      clearShownItem
      runDB $ DB.delete ci
