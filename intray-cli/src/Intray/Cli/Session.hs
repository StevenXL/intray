module Intray.Cli.Session
  ( withToken,
    loadToken,
    clearSession,
    loadSession,
    saveSession,
  )
where

import qualified Data.ByteString as SB
import qualified Data.ByteString.Builder as SBB
import qualified Data.ByteString.Lazy as LB
import Import
import Intray.Cli.Env
import Intray.Cli.Path
import Servant.Auth.Client
import Web.Cookie

withToken :: (Token -> CliM a) -> CliM a
withToken func = do
  mToken <- loadToken
  case mToken of
    Nothing -> liftIO $ die "Please log in first"
    Just token -> func token

loadToken :: CliM (Maybe Token)
loadToken = do
  mCookie <- loadSession
  pure $ Token . setCookieValue <$> mCookie

clearSession :: CliM ()
clearSession = do
  p <- sessionPath
  liftIO $ ignoringAbsence $ removeFile p

loadSession :: CliM (Maybe SetCookie)
loadSession = do
  p <- sessionPath
  mContents <- liftIO $ forgivingAbsence $ SB.readFile $ toFilePath p
  pure $ parseSetCookie <$> mContents

saveSession :: SetCookie -> CliM ()
saveSession setCookie = do
  p <- sessionPath
  liftIO $ do
    ensureDir $ parent p
    LB.writeFile (toFilePath p) $ SBB.toLazyByteString $ renderSetCookie setCookie
