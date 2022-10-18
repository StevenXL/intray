{-# LANGUAGE RecordWildCards #-}

module Intray.Cli.Sqlite
  ( clearShownItem,
    getShownItem,
    produceShownItem,
    getStoreSize,
    makeSyncRequest,
    mergeSyncResponse,
  )
where

import Data.Mergeless (SyncRequest, SyncResponse)
import Data.Mergeless.Persistent as Mergeless
import Database.Persist.Sqlite
import Import
import Intray.API
import Intray.Cli.DB
import Intray.Cli.Env

setShownItem :: ClientItemId -> CliM ()
setShownItem clientItemId =
  void $
    runDB $
      upsertBy
        (UniqueShownItem clientItemId)
        (ShownItem {shownItemItem = clientItemId})
        [ShownItemItem =. clientItemId]

clearShownItem :: CliM ()
clearShownItem = runDB $ deleteWhere ([] :: [Filter ShownItem])

getShownItem :: CliM (Maybe ClientItemId)
getShownItem = runDB $ fmap (shownItemItem . entityVal) <$> selectFirst [] []

produceShownItem :: CliM (Maybe (Entity ClientItem))
produceShownItem = do
  mShownItemId <- getShownItem
  mShownItem <- fmap join $
    forM mShownItemId $ \shownItem -> do
      clientItem <- runDB $ get shownItem
      pure (Entity shownItem <$> clientItem)
  case mShownItem of
    Just clientItem -> pure (Just clientItem)
    Nothing -> do
      mClientItem <- runDB $ selectFirst [] []
      forM mClientItem $ \clientItemEntity@(Entity clientItemId _) -> do
        setShownItem clientItemId
        pure clientItemEntity

getStoreSize :: CliM Int
getStoreSize = runDB $ count ([] :: [Filter ClientItem])

makeSyncRequest :: CliM (SyncRequest ClientItemId ItemUUID (AddedItem TypedItem))
makeSyncRequest = runDB $ Mergeless.clientMakeSyncRequestQuery from ClientItemServerIdentifier ClientItemDeleted
  where
    from :: ClientItem -> AddedItem TypedItem
    from ClientItem {..} =
      AddedItem
        { addedItemCreated = clientItemCreated,
          addedItemContents =
            TypedItem
              { itemType = clientItemType,
                itemData = clientItemContents
              }
        }

mergeSyncResponse :: SyncResponse ClientItemId ItemUUID (AddedItem TypedItem) -> CliM ()
mergeSyncResponse = runDB . Mergeless.clientMergeSyncResponseQuery to ClientItemServerIdentifier ClientItemDeleted
  where
    to :: ItemUUID -> AddedItem TypedItem -> ClientItem
    to serverId AddedItem {..} =
      let TypedItem {..} = addedItemContents
       in ClientItem
            { clientItemType = itemType,
              clientItemContents = itemData,
              clientItemCreated = addedItemCreated,
              clientItemServerIdentifier = Just serverId,
              clientItemDeleted = False
            }
