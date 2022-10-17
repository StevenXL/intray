{-# LANGUAGE OverloadedStrings #-}

module Intray.Cli.Commands.Review (review) where

import Control.Monad.Logger
import qualified Data.Text as T
import Data.Time
import Database.Persist
import Import
import Intray.Cli.Commands.Done
import Intray.Cli.Env
import Intray.Cli.OptParse
import Intray.Cli.Prompt
import Intray.Cli.Sqlite
import Intray.Cli.Store
import Intray.Cli.Sync

review :: CliM ()
review = do
  autoSyncStore
  mShownItem <- produceShownItem
  case mShownItem of
    Nothing -> logInfoN "Done."
    Just clientItem -> do
      now <- liftIO getCurrentTime
      s <- getStoreSize
      logInfoN $ T.pack $ unwords [show s, "items remaining"]
      prettyShowItemAndWait now clientItem
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
