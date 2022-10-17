module Intray.Cli.Env where

import Control.Monad.IO.Class
import Control.Monad.Logger
import Control.Monad.Reader
import Database.Persist
import Database.Persist.Sql
import Database.Persist.Sqlite
import Import
import Intray.Cli.OptParse.Types
import Servant.Client

type CliM = ReaderT Env (LoggingT IO)

data Env = Env
  { envDataDir :: !(Path Abs Dir),
    envCacheDir :: !(Path Abs Dir),
    envAutoOpen :: !AutoOpen,
    envSyncStrategy :: !SyncStrategy,
    envConnectionPool :: !ConnectionPool,
    envClientEnv :: !(Maybe ClientEnv)
  }

runDB :: SqlPersistT (LoggingT IO) a -> CliM a
runDB query = do
  pool <- asks envConnectionPool
  logFunc <- askLoggerIO
  liftIO $ runLoggingT (runSqlPool (retryOnBusy query) pool) logFunc
