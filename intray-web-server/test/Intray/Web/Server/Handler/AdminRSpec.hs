{-# LANGUAGE OverloadedStrings #-}

module Intray.Web.Server.Handler.AdminRSpec where

import Intray.Web.Server.Foundation
import Intray.Web.Server.TestUtils
import Test.Syd.Yesod
import TestImport

{-# ANN module ("HLint: ignore Reduce duplication" :: String) #-}

spec :: Spec
spec = intrayWebServerSpec $ do
  describe "AdminR" $ do
    it "gets a 200 when logged in as admin" $
      withAdminAccount_ $ do
        get AdminR
        statusIs 200
    it "gets a 404 when not logged in as admin" $
      withExampleAccount_ $ do
        get AdminR
        statusIs 404

  describe "AdminAccountR" $ do
    it "gets a 200 when logged in as admin" $
      withAdminAccount_ $ do
        get AdminAccountR
        statusIs 200
    it "gets a 404 when not logged in as admin" $
      withExampleAccount_ $ do
        get AdminAccountR
        statusIs 404
