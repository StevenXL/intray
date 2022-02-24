{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module Intray.Server.OptParse
  ( module Intray.Server.OptParse,
    module Intray.Server.OptParse.Types,
  )
where

import Autodocodec.Yaml
import Control.Monad.Logger
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Database.Persist.Sqlite
import qualified Env
import Import
import Intray.API
import Intray.Server.OptParse.Types
import Looper
import Options.Applicative
import qualified Options.Applicative.Help as OptParse
import qualified System.Environment as System
import Web.Stripe.Client as Stripe
import Web.Stripe.Types as Stripe

getSettings :: IO Settings
getSettings = do
  flags <- getFlags
  env <- getEnvironment
  config <- getConfiguration flags env
  combineToSettings flags env config

combineToSettings :: Flags -> Environment -> Maybe Configuration -> IO Settings
combineToSettings Flags {..} Environment {..} mConf = do
  let mc :: (Configuration -> Maybe a) -> Maybe a
      mc func = mConf >>= func
  let setPort = fromMaybe 8000 $ flagPort <|> envPort <|> mc confPort
  let setHost =
        T.pack $ fromMaybe ("localhost:" <> show setPort) $ flagHost <|> envHost <|> mc confHost
  let setLogLevel = fromMaybe LevelInfo $ flagLogLevel <|> envLogLevel <|> mc confLogLevel
  setSigningKeyFile <-
    case flagSigningKeyFile <|> envSigningKeyFile <|> mc confSigningKeyFile of
      Nothing -> resolveFile' "signing-key.json"
      Just skf -> resolveFile' skf
  let setConnectionInfo = mkSqliteConnectionInfo $ fromMaybe "intray.db" (flagDb <|> envDb <|> mc confDb)
  setAdmins <-
    forM (flagAdmins ++ fromMaybe [] (mc confAdmins)) $ \s ->
      case parseUsername $ T.pack s of
        Nothing -> die $ unwords ["Invalid admin username:", s]
        Just u -> pure u
  setFreeloaders <-
    forM (flagFreeloaders ++ fromMaybe [] (mc confFreeloaders)) $ \s ->
      case parseUsername $ T.pack s of
        Nothing -> die $ unwords ["Invalid freeloader username:", s]
        Just u -> pure u
  setMonetisationSettings <-
    do
      let mmc :: (MonetisationConfiguration -> Maybe a) -> Maybe a
          mmc func = mc confMonetisationConfig >>= func
      let plan =
            Stripe.PlanId . T.pack
              <$> (flagStripePlan <|> envStripePlan <|> mmc monetisationConfStripePlan)
      let config =
            ( \sk ->
                StripeConfig
                  { Stripe.secretKey = StripeKey $ TE.encodeUtf8 $ T.pack sk,
                    stripeEndpoint = Nothing
                  }
            )
              <$> ( flagStripeSecretKey <|> envStripeSecretKey
                      <|> mmc monetisationConfStripeSecretKey
                  )
      let publicKey =
            T.pack
              <$> ( flagStripePublishableKey <|> envStripePublishableKey
                      <|> mmc monetisationConfStripePublishableKey
                  )
      let fetcherSets =
            deriveLooperSettings
              (seconds 0)
              (minutes 1)
              flagLooperStripeEventsFetcher
              envLooperStripeEventsFetcher
              (mmc monetisationConfStripeEventsFetcher)
      let retrierSets =
            deriveLooperSettings
              (seconds 30)
              (hours 24)
              flagLooperStripeEventsRetrier
              envLooperStripeEventsRetrier
              (mmc monetisationConfStripeEventsRetrier)
      let maxItemsFree =
            fromMaybe 5 $
              flagMaxItemsFree <|> envMaxItemsFree <|> mmc monetisationConfMaxItemsFree
      pure $ do
        ss <- StripeSettings <$> plan <*> config <*> publicKey
        pure $
          MonetisationSettings
            { monetisationSetStripeSettings = ss,
              monetisationSetStripeEventsFetcher = fetcherSets,
              monetisationSetStripeEventsRetrier = retrierSets,
              monetisationSetMaxItemsFree = maxItemsFree
            }
  pure Settings {..}

getConfiguration :: Flags -> Environment -> IO (Maybe Configuration)
getConfiguration Flags {..} Environment {..} = do
  cp <-
    case flagConfigFile <|> envConfigFile of
      Nothing -> getDefaultConfigFile
      Just cf -> resolveFile' cf
  readYamlConfigFile cp

getDefaultConfigFile :: IO (Path Abs File)
getDefaultConfigFile = resolveFile' "config.yaml"

getEnvironment :: IO Environment
getEnvironment = Env.parse id environmentParser

environmentParser :: Env.Parser Env.Error Environment
environmentParser =
  Env.prefixed "INTRAY_SERVER_" $
    Environment <$> Env.var (fmap Just . Env.str) "CONFIG_FILE" (mE "Config file")
      <*> Env.var (fmap Just . Env.str) "HOST" (mE "host to run the api server on")
      <*> Env.var (fmap Just . Env.auto) "PORT" (mE "port to run the api server on")
      <*> Env.var (fmap Just . Env.str) "DATABASE" (mE "database file")
      <*> Env.var (fmap Just . Env.auto) "LOG_LEVEL" (mE "minimal severity of log messages")
      <*> Env.var (fmap Just . Env.str) "SIGNING_KEY_FILE" (mE "the file to store the signing key in")
      <*> Env.var (fmap Just . Env.str) "STRIPE_PLAN" (mE "stripe plan id for subscriptions")
      <*> Env.var (fmap Just . Env.str) "STRIPE_SECRET_KEY" (mE "stripe secret key")
      <*> Env.var (fmap Just . Env.str) "STRIPE_PUBLISHABLE_KEY" (mE "stripe publishable key")
      <*> looperVarEnv "STRIPE_EVENTS_FETCHER"
      <*> looperVarEnv "STRIPE_EVENTS_RETRIER"
      <*> Env.var (fmap Just . Env.auto) "MAX_ITEMS_FREE" (mE "maximum items that a free user can have")
  where
    mE h = Env.def Nothing <> Env.keep <> Env.help h

looperVarEnv :: String -> Env.Parser Env.Error LooperEnvironment
looperVarEnv n = Env.prefixed "LOOPER_" $ looperEnvironmentParser n

getFlags :: IO Flags
getFlags = do
  args <- System.getArgs
  let result = runFlagsParser args
  handleParseResult result

runFlagsParser :: [String] -> ParserResult Flags
runFlagsParser = execParserPure prefs_ flagsParser
  where
    prefs_ =
      defaultPrefs
        { prefShowHelpOnError = True,
          prefShowHelpOnEmpty = True
        }

flagsParser :: ParserInfo Flags
flagsParser = info (helper <*> parseFlags) (fullDesc <> footerDoc (Just $ OptParse.string footerStr))
  where
    footerStr =
      unlines
        [ Env.helpDoc environmentParser,
          "",
          "Configuration file format:",
          T.unpack (TE.decodeUtf8 (renderColouredSchemaViaCodec @Configuration))
        ]

parseFlags :: Parser Flags
parseFlags =
  Flags
    <$> option
      (Just <$> str)
      (mconcat [long "config-file", value Nothing, metavar "FILEPATH", help "The config file"])
    <*> option
      (Just <$> str)
      (mconcat [long "host", value Nothing, metavar "HOST", help "the host to serve on"])
    <*> option
      (Just <$> auto)
      (mconcat [long "port", value Nothing, metavar "PORT", help "the port to serve on"])
    <*> option
      (Just . T.pack <$> str)
      ( mconcat
          [ long "database",
            value Nothing,
            metavar "DATABASE_CONNECTION_STRING",
            help "The sqlite connection string"
          ]
      )
    <*> many (strOption (mconcat [long "admin", metavar "USERNAME", help "An admin"]))
    <*> many (strOption (mconcat [long "freeloader", metavar "USERNAME", help "A freeloader"]))
    <*> option
      (Just <$> auto)
      ( mconcat
          [ long "log-level",
            metavar "LOG_LEVEL",
            value Nothing,
            help $
              "the log level, possible values: " <> show [LevelDebug, LevelInfo, LevelWarn, LevelError]
          ]
      )
    <*> option
      (Just <$> str)
      ( mconcat
          [ long "signing-key-file",
            value Nothing,
            metavar "FILEPATH",
            help "the file to store the signing key in"
          ]
      )
    <*> option
      (Just <$> str)
      ( mconcat
          [ long "stripe-plan",
            value Nothing,
            metavar "PLAN_ID",
            help "The product pricing plan for stripe"
          ]
      )
    <*> option
      (Just <$> str)
      ( mconcat
          [ long "stripe-secret-key",
            value Nothing,
            metavar "SECRET_KEY",
            help "The secret key for stripe"
          ]
      )
    <*> option
      (Just <$> str)
      ( mconcat
          [ long "stripe-publishable-key",
            value Nothing,
            metavar "PUBLISHABLE_KEY",
            help "The publishable key for stripe"
          ]
      )
    <*> getLooperFlags "stripe-events-fetcher"
    <*> getLooperFlags "stripe-events-retrier"
    <*> option
      (Just <$> auto)
      ( mconcat
          [ long "max-items-free",
            value Nothing,
            metavar "INT",
            help "How many items a user can sync in the free plan"
          ]
      )
