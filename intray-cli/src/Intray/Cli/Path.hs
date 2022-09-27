module Intray.Cli.Path
  ( getDBPath,
    getDBLockPath,
    sessionPath,
    lastSeenItemPath,
    storePath,
  )
where

import Import
import Intray.Cli.OptParse

getDBPath :: CliM (Path Abs File)
getDBPath = do
  d <- asks setDataDir
  resolveFile d "intray.sqlite3"

getDBLockPath :: CliM (Path Abs File)
getDBLockPath = do
  d <- asks setDataDir
  resolveFile d "intray.sqlite3.lock"

sessionPath :: CliM (Path Abs File)
sessionPath = do
  d <- asks setCacheDir
  resolveFile d "session.cookie"

lastSeenItemPath :: CliM (Path Abs File)
lastSeenItemPath = do
  d <- asks setCacheDir
  resolveFile d "last-seen-item.json"

storePath :: CliM (Path Abs File)
storePath = do
  d <- asks setDataDir
  resolveFile d "store.json"
