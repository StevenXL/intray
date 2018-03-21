{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DataKinds #-}

module Intray.Server.Handler.AdminGetAccounts
    ( serveAdminGetAccounts
    ) where

import Import

import Database.Persist

import Servant hiding (BadPassword, NoSuchUser)
import Servant.Auth.Server as Auth
import Servant.Auth.Server.SetCookieOrphan ()

import Intray.API
import Intray.Data

import Intray.Server.Types

import Intray.Server.Handler.Utils

serveAdminGetAccounts :: AuthResult AuthCookie -> IntrayHandler [AccountInfo]
serveAdminGetAccounts (Authenticated AuthCookie {..}) =
    withAdminCreds authCookieUserUuid $ do
        admins <- asks envAdmins
        users <- runDb $ selectList [] [Asc UserId]
        pure $
            flip map users $ \(Entity _ User {..}) ->
                AccountInfo
                { accountInfoUuid = userIdentifier
                , accountInfoUsername = userUsername
                , accountInfoCreatedTimestamp = userCreatedTimestamp
                , accountInfoAdmin = userUsername `elem` admins
                }
serveAdminGetAccounts _ = throwAll err401
