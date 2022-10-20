{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

module Intray.Cli.Commands.Show (showItem) where

import Data.Time
import Import
import Intray.Cli.Env
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
    Just item -> do
      ao <- asks envAutoOpen
      cd <- asks envCacheDir
      liftIO $ prettyShowItemAndWait ao cd now item
