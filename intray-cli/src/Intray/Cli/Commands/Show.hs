{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

module Intray.Cli.Commands.Show (showItem) where

import Data.Time
import Database.Persist
import Import
import Intray.Cli.DB
import Intray.Cli.OptParse
import Intray.Cli.Sqlite
import Intray.Cli.Store

showItem :: CliM ()
showItem = do
  pure ()
