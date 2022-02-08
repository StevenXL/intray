{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}

module Intray.Server.Handler.Admin.GetAccounts
  ( serveAdminGetAccounts,
  )
where

import Database.Persist
import Import
import Intray.API
import Intray.Server.Handler.Admin.GetAccount
import Intray.Server.Handler.Utils
import Intray.Server.Types

serveAdminGetAccounts :: AuthCookie -> IntrayHandler [AccountInfo]
serveAdminGetAccounts AuthCookie {..} = do
  users <- runDb $ selectList [] [Desc UserLastLogin]
  forM users $ \(Entity _ user) -> getAccountInfoForUser user
