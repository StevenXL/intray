{-# LANGUAGE LambdaCase #-}

module Intray.Cli.Client where

import Import
import Intray.Cli.Env
import Intray.Cli.OptParse
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Servant.Client

runSingleClientOrErr :: ClientM a -> CliM (Maybe a)
runSingleClientOrErr func = do
  mErrOrRes <- runSingleClient func
  forM mErrOrRes $ \case
    Left err -> liftIO $ die $ unlines ["Error while contacting the intray server:", show err]
    Right r -> pure r

runSingleClient :: ClientM a -> CliM (Maybe (Either ClientError a))
runSingleClient func = do
  mCenv <- asks envClientEnv
  forM mCenv $ \cenv -> liftIO $ runClientM func cenv
