{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}

module Intray.Cli.Commands.Register (register) where

import Import
import Intray.API
import Intray.Cli.Client
import Intray.Cli.OptParse
import Intray.Cli.Prompt
import Intray.Client

register :: RegisterSettings -> CliM ()
register RegisterSettings {..} = do
  registration <-
    Registration <$> promptUsername registerSetUsername <*> promptPassword registerSetPassword
  mRes <- runSingleClientOrErr $ clientPostRegister registration
  case mRes of
    Nothing -> liftIO $ die "No server configured."
    Just NoContent -> pure ()
