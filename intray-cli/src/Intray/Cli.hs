{-# LANGUAGE RecordWildCards #-}

module Intray.Cli (intrayCli, dispatch) where

import Control.Monad.Logger
import qualified Data.Text as T
import Database.Persist.Sql
import Database.Persist.Sqlite
import Import
import Intray.Cli.Commands
import Intray.Cli.DB
import Intray.Cli.Env
import Intray.Cli.OptParse
import Network.HTTP.Client as HTTP
import Network.HTTP.Client.TLS as HTTP
import Servant.Client
import System.FileLock

intrayCli :: IO ()
intrayCli = do
  Instructions disp Settings {..} <- getInstructions

  dbPath <- resolveFile setDataDir "intray.sqlite3"
  dbLockPath <- resolveFile setDataDir "intray.sqlite3.lock"

  ensureDir (parent dbPath)
  withFileLock (fromAbsFile dbLockPath) Exclusive $ \_ -> do
    runStderrLoggingT . filterLogger (\_ ll -> ll >= setLogLevel) $
      withSqlitePoolInfo (mkSqliteConnectionInfo $ T.pack $ fromAbsFile dbPath) 1 $ \pool -> do
        flip runSqlPool pool $ do
          _ <- runMigrationQuiet clientAutoMigration
          pure ()

        mClientEnv <- forM setBaseUrl $ \burl -> do
          man <- liftIO $ HTTP.newManager tlsManagerSettings
          pure $ mkClientEnv man burl

        runReaderT
          (dispatch disp)
          Env
            { envDataDir = setDataDir,
              envCacheDir = setCacheDir,
              envAutoOpen = setAutoOpen,
              envSyncStrategy = setSyncStrategy,
              envConnectionPool = pool,
              envClientEnv = mClientEnv
            }

dispatch :: Dispatch -> CliM ()
dispatch d =
  case d of
    DispatchRegister rs -> register rs
    DispatchLogin ls -> login ls
    DispatchAddItem t -> addItem t
    DispatchShowItem -> showItem
    DispatchDoneItem -> doneItem
    DispatchSize -> size
    DispatchReview -> review
    DispatchLogout -> logout
    DispatchSync -> sync
