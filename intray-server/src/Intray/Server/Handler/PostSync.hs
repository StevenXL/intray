{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}

module Intray.Server.Handler.PostSync
  ( servePostSync,
  )
where

import qualified Data.Map as M
import Data.Mergeless
import Data.Mergeless.Persistent
import Data.UUID.Typed
import Database.Persist
import Import
import Intray.API
import Intray.Server.Handler.Stripe
import Intray.Server.Handler.Utils
import Intray.Server.Item
import Intray.Server.Types

servePostSync ::
  AuthCookie ->
  SyncRequest ClientItemId ItemUUID (AddedItem TypedItem) ->
  IntrayHandler (SyncResponse ClientItemId ItemUUID (AddedItem TypedItem))
servePostSync AuthCookie {..} sr = do
  ps <- getUserPaidStatus authCookieUserUUID
  doSync ps authCookieUserUUID sr

doSync ::
  PaidStatus ->
  AccountUUID ->
  SyncRequest ClientItemId ItemUUID (AddedItem TypedItem) ->
  IntrayHandler (SyncResponse ClientItemId ItemUUID (AddedItem TypedItem))
doSync ps userId =
  runDB
    . serverProcessSyncWithCustomIdQuery
      nextRandomUUID
      IntrayItemIdentifier
      [IntrayItemUserId ==. userId]
      makeAdded
      (makeAddedIntrayItem userId)
    . modifySyncRequest
  where
    modifySyncRequest sr =
      let modAddedFunc =
            case ps of
              HasNotPaid i -> M.fromList . take i . M.toList
              HasPaid _ -> id
              NoPaymentNecessary -> id
       in sr {syncRequestAdded = modAddedFunc $ syncRequestAdded sr}
