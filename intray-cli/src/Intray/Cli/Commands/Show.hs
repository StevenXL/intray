{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

module Intray.Cli.Commands.Show (showItem) where

import Data.Time
import Import
import Intray.Cli.OptParse
import Intray.Cli.Store
import Intray.Cli.Sync

showItem :: CliM ()
showItem = do
  mls <- readLastSeen
  mli <-
    case mls of
      Nothing -> syncAndReturn lastItemInClientStore
      Just li -> pure $ Just li
  case mli of
    Nothing -> liftIO $ putStrLn "Done."
    Just li -> do
      writeLastSeen li
      now <- liftIO getCurrentTime
      prettyShowItemAndWait now li
