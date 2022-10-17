module Intray.Cli.Path
  ( sessionPath,
    lastSeenItemPath,
    storePath,
  )
where

import Import
import Intray.Cli.Env

sessionPath :: CliM (Path Abs File)
sessionPath = do
  d <- asks envCacheDir
  resolveFile d "session.cookie"

lastSeenItemPath :: CliM (Path Abs File)
lastSeenItemPath = do
  d <- asks envCacheDir
  resolveFile d "last-seen-item.json"

storePath :: CliM (Path Abs File)
storePath = do
  d <- asks envDataDir
  resolveFile d "store.json"
