{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Intray.Cli.Commands.Add (addItem) where

import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Time
import Import
import Intray.API
import Intray.Cli.Client
import Intray.Cli.OptParse
import Intray.Cli.Session
import Intray.Cli.Store
import Intray.Cli.Sync
import Intray.Client

addItem :: AddSettings -> CliM ()
addItem AddSettings {..} = do
  now <- liftIO getCurrentTime
  mItemContents <-
    case (addSetReadStdin, addSetContents) of
      (False, []) -> pure Nothing
      (True, []) -> Just <$> liftIO T.getContents
      (False, cts) -> pure $ Just $ T.unwords cts
      (True, cts) ->
        Just <$> do
          cts' <- liftIO T.getContents
          pure $ T.intercalate "\n" [T.unwords cts, cts']
  forM_ mItemContents $ \contents -> do
    let ti = textTypedItem contents
    let modStore :: CS -> CS
        modStore = addItemToClientStore AddedItem {addedItemContents = ti, addedItemCreated = now}
    if addSetRemote
      then withToken $ \token -> do
        mr <- runSingleClientOrErr $ clientPostAddItem token ti
        case mr of
          Nothing -> liftIO $ die "Not logged in."
          Just _ -> pure ()
      else modifyClientStoreAndSync modStore
