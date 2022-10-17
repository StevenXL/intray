{-# LANGUAGE RecordWildCards #-}

module Intray.Cli (intrayCli) where

import Control.Monad.Logger
import qualified Data.Text as T
import Import
import Intray.Cli.Commands
import Intray.Cli.Env
import Intray.Cli.OptParse
import System.FileLock

intrayCli :: IO ()
intrayCli = do
  Instructions disp Settings {..} <- getInstructions

  dbPath <- resolveFile setDataDir "intray.sqlite3"
  dbLockPath <- resolveFile setDataDir "intray.sqlite3.lock"

  withFileLock dbLockPath Exclusive $ \_ -> do
    withSqlitePoolInfo (mkSqliteConnectionInfo $ T.pack $ fromAbsFile dbFile) 1 $ \pool ->
      flip runSqlPool pool $ do
        _ <- runMigrationQuiet clientAutoMigration
        func

    let mBurl = setBaseUrl set
    mClientEnv <- forM mBurl $ \burl -> do
      man <- newManager tlsManagerSettings
      pure $ mkClientEnv man burl

    runReaderT (dispatch disp) env

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
