module Intray.Cli.Commands.Logout (logout) where

import Import
import Intray.Cli.OptParse
import Intray.Cli.Path

logout :: CliM ()
logout = do
  p <- sessionPath
  liftIO $ ignoringAbsence $ removeFile p
