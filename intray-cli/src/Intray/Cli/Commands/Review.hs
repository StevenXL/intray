{-# LANGUAGE OverloadedStrings #-}

module Intray.Cli.Commands.Review (review) where

import Data.Time
import Import
import Intray.Cli.Commands.Done
import Intray.Cli.OptParse
import Intray.Cli.Prompt
import Intray.Cli.Store
import Intray.Cli.Sync

review :: CliM ()
review = do
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
      let showSize = do
            s <- syncAndReturn storeSize
            liftIO $ putStrLn $ unwords [show s, "items remaining"]
      showSize
      prettyShowItemAndWait now li
      res <- liftIO $ prompt "done [y/N]"
      let cont = do
            doneItem
            review
          stop = pure ()
      case res of
        "y" -> cont
        "Y" -> cont
        "n" -> stop
        "N" -> stop
        "" -> stop
        _ -> review
