{-# LANGUAGE RecordWildCards #-}

module Intray.Cli.Store
  ( prettyShowItemAndWait,
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
import Intray.Cli.Env
import Intray.Cli.OptParse
import Intray.Cli.Path
import Network.URI
import System.FileLock
import System.Process.Typed
import Text.Time.Pretty
import UnliftIO.Exception

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
                cacheDir <- asks envCacheDir
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
  aa <- asks envAutoOpen
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
