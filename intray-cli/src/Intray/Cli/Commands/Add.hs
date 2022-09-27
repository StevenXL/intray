{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Intray.Cli.Commands.Add (addItem) where

import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.IO as T
import Data.Time
import qualified Database.Persist.Sql as DB
import Import
import Intray.API
import Intray.Cli.Client
import Intray.Cli.DB
import Intray.Cli.OptParse
import Intray.Cli.Session
import Intray.Cli.Sqlite
import Intray.Client

addItem :: AddSettings -> CliM ()
addItem addSettings@AddSettings {..} = do
  mItemContents <- liftIO $ getItemContents addSettings
  forM_ mItemContents $ \contents -> do
    if addSetRemote
      then addItemRemotely contents
      else addItemLocally contents

getItemContents :: AddSettings -> IO (Maybe Text)
getItemContents AddSettings {..} =
  case (addSetReadStdin, addSetContents) of
    (False, []) -> pure Nothing
    (True, []) -> Just <$> liftIO T.getContents
    (False, cts) -> pure $ Just $ T.unwords cts
    (True, cts) ->
      Just <$> do
        cts' <- liftIO T.getContents
        pure $ T.intercalate "\n" [T.unwords cts, cts']

addItemRemotely :: Text -> CliM ()
addItemRemotely contents = do
  let ti = textTypedItem contents
  withToken $ \token -> do
    mr <- runSingleClientOrErr $ clientPostAddItem token ti
    case mr of
      Nothing -> liftIO $ die "Not logged in."
      Just _ -> pure ()

addItemLocally :: Text -> CliM ()
addItemLocally contents = withDB $ do
  now <- liftIO getCurrentTime
  let ci =
        ClientItem
          { clientItemType = TextItem,
            clientItemContents = TE.encodeUtf8 contents,
            clientItemCreated = now,
            clientItemServerIdentifier = Nothing
          }
  DB.insert_ ci
