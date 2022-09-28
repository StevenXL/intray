{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}

module Intray.Cli.Store
  ( CS,
    ClientStore (..),
    withClientStore,
    withClientStore',
    readClientStore,
    -- readClientStoreOrEmpty,
    readClientStoreSize,
    -- writeClientStore,
    addItemToClientStore,
    storeSize,
    anyUnsynced,
    writeLastSeen,
    readLastSeen,
    clearLastSeen,
    prettyShowItemAndWait,
  )
where

import qualified Data.ByteString as SB
import qualified Data.Map as M
import Data.Mergeless
import qualified Data.Text as T
import Data.Time
import Database.Persist
import Database.Persist.Sql
import Import
import Intray.API
import Intray.Cli.DB
import Intray.Cli.JSON
import Intray.Cli.OptParse
import Intray.Cli.Path
import Network.URI
import System.FileLock
import System.Process.Typed
import Text.Time.Pretty
import UnliftIO.Exception

type CS = ClientStore ClientId ItemUUID (AddedItem TypedItem)

withClientStore :: (CS -> CliM CS) -> CliM ()
withClientStore func = void $ withClientStore' (fmap ((,) ()) . func)

withClientStore' :: (CS -> CliM (a, CS)) -> CliM a
withClientStore' func = do
  p <- storeLockPath
  bracket
    ( liftIO $ do
        ensureDir (parent p)
        lockFile (fromAbsFile p) Exclusive
    )
    (liftIO . unlockFile)
    $ \_ -> do
      before <- readClientStoreOrEmpty
      (r, after) <- func before
      writeClientStore after
      pure r

storeLockPath :: CliM (Path Abs File)
storeLockPath = do
  p <- storePath
  resolveFile' $ fromAbsFile p ++ ".lock"

readClientStore :: CliM (Maybe CS)
readClientStore = do
  p <- storePath
  readJSON p $
    unlines
      [ "If you see this error, it means the way serialisation of the store has changed in a backward-incompatible way.",
        "As long as you have no unsynced items, you can just remove this file and re-sync: "
          <> fromAbsFile p,
        "If you do have unsynced items, you will want to make a backup of this file first."
      ]

readClientStoreOrEmpty :: CliM CS
readClientStoreOrEmpty = fromMaybe emptyClientStore <$> readClientStore

readClientStoreSize :: CliM Int
readClientStoreSize = storeSize <$> readClientStoreOrEmpty

writeClientStore :: CS -> CliM ()
writeClientStore s = do
  storePath >>= (`writeJSON` s)

anyUnsynced :: CS -> Bool
anyUnsynced = not . M.null . clientStoreAdded

readLastSeen :: CliM (Maybe ClientItemId)
readLastSeen = do
  p <- lastSeenItemPath
  readJSON p $
    unlines
      [ "If you see this error, it means that the way serialisation of the last-seen item cache has changed in a backward-incompatible way.",
        "You can just remove this file and everything else will work as intended: " <> fromAbsFile p
      ]

writeLastSeen :: ClientId -> CliM ()
writeLastSeen i = do
  p <- lastSeenItemPath
  writeJSON p i

clearLastSeen :: CliM ()
clearLastSeen = do
  p <- lastSeenItemPath
  liftIO $ ignoringAbsence $ removeFile p

prettyShowItemAndWait :: UTCTime -> Entity ClientItem -> CliM ()
prettyShowItemAndWait now (Entity cid ClientItem {..}) =
  let idString = show (fromSqlKey cid)
      timeStr = prettyTimestamp now clientItemCreated
      timeAgoString = prettyTimeAuto now clientItemCreated
      typedItem = TypedItem clientItemType clientItemContents
   in case typedItemCase typedItem of
        Left err -> liftIO $ putStrLn $ unlines ["Invalid item:", err]
        Right i -> do
          (contents, mp) <-
            case i of
              CaseTextItem t -> do
                mp <-
                  fmap join $
                    forM (parseURI (T.unpack t)) $
                      \uri ->
                        getAutoOpenConfig (show uri)
                pure (T.unpack t, mp)
              CaseImageItem it bs -> do
                let ext =
                      case it of
                        JpgImage -> ".jpg"
                        PngImage -> ".png"
                cacheDir <- asks setCacheDir
                let fileName = idString ++ ext
                file <- resolveFile cacheDir fileName
                liftIO $ SB.writeFile (fromAbsFile file) bs
                mp <- getAutoOpenConfig (fromAbsFile file)
                pure ("Image: " <> fromAbsFile file, mp)
          let waitFunc = case mp of
                Nothing -> id
                Just pc -> withProcessWait pc . const
          waitFunc $ liftIO $ putStrLn $ unlines [concat [timeStr, " (", timeAgoString, ")"], contents]

getAutoOpenConfig :: String -> CliM (Maybe (ProcessConfig () () ()))
getAutoOpenConfig arg = do
  aa <- asks setAutoOpen
  pure $ case aa of
    DontAutoOpen -> Nothing
    AutoOpenWith cmd -> Just (autoOpenConfig cmd arg)

autoOpenConfig :: FilePath -> String -> ProcessConfig () () ()
autoOpenConfig cmd arg = shell $ unwords [cmd, arg]

prettyTimestamp :: UTCTime -> UTCTime -> String
prettyTimestamp now d =
  let year = (\(y, _, _) -> y) . toGregorian . utctDay
   in ( if year now == year d
          then formatTime defaultTimeLocale "%A %B %e at %H:%M"
          else formatTime defaultTimeLocale "%A %B %e %Y at %H:%M"
      )
        d
