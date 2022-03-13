{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Intray.Server.TestUtils
  ( withIntrayServer,
    withFreeIntrayServer,
    withPaidIntrayServer,
    intrayTestConnectionSetupFunc,
    intrayTestClientEnvSetupFunc,
    intrayTestClientEnvSetupFunc',
    runClient,
    runClientOrError,
    randomRegistration,
    withAdmin,
    withValidNewUser,
    withValidNewUserAndData,
    requiresAdmin,
    withNewUser'sAccessKey,
    login,
    failsWithOutPermissions,
    failsWithOutPermission,
    module Servant.Client,
  )
where

import Control.Monad.Logger
import Data.Set (Set)
import qualified Data.Set as S
import Data.Text.Encoding (encodeUtf8)
import Data.UUID.Typed
import Database.Persist.Sqlite
import Import
import Intray.API
import Intray.Client
import Intray.Data.Gen ()
import Intray.Server
import Intray.Server.OptParse.Types
import Intray.Server.Types
import qualified Network.HTTP.Client as HTTP
import qualified Network.HTTP.Types as HTTP
import Servant
import Servant.Auth.Server as Auth
import Servant.Client
import Test.Syd.Persistent.Sqlite
import Test.Syd.Wai
import Web.Cookie

{-# ANN module ("HLint: ignore Use camelCase" :: String) #-}

withIntrayServer :: TestDef '[HTTP.Manager] ClientEnv -> Spec
withIntrayServer specFunc = do
  describe "Paying" $ withPaidIntrayServer 5 specFunc
  describe "Free" $ withFreeIntrayServer specFunc

withPaidIntrayServer :: Int -> TestDef '[HTTP.Manager] ClientEnv -> Spec
withPaidIntrayServer maxFree specFunc =
  managerSpec $
    setupAroundWith' (\man () -> paidIntrayTestClientEnvSetupFunc maxFree man) $
      modifyMaxSuccess (`div` 20) specFunc

withFreeIntrayServer :: TestDef '[HTTP.Manager] ClientEnv -> Spec
withFreeIntrayServer specFunc =
  managerSpec $
    setupAroundWith' (\man () -> intrayTestClientEnvSetupFunc Nothing man) $
      modifyMaxSuccess (`div` 20) specFunc

intrayTestConnectionSetupFunc :: SetupFunc ConnectionPool
intrayTestConnectionSetupFunc = connectionPoolSetupFunc migrateAll

paidIntrayTestClientEnvSetupFunc :: Int -> HTTP.Manager -> SetupFunc ClientEnv
paidIntrayTestClientEnvSetupFunc maxFree man = do
  let monetisationSetStripeSettings =
        StripeSettings
          { stripeSetPlan = "dummy-plan",
            stripeSetSecretKey = error "should not try to access stripe during testing",
            stripeSetPublishableKey = "Example, should not be used."
          }
  let monetisationSetPrice = "dummy price"
  let monetisationSetMaxItemsFree = maxFree
  intrayTestClientEnvSetupFunc (Just MonetisationSettings {..}) man

intrayTestClientEnvSetupFunc :: Maybe MonetisationSettings -> HTTP.Manager -> SetupFunc ClientEnv
intrayTestClientEnvSetupFunc menv man = intrayTestConnectionSetupFunc >>= intrayTestClientEnvSetupFunc' menv man

intrayTestClientEnvSetupFunc' :: Maybe MonetisationSettings -> HTTP.Manager -> ConnectionPool -> SetupFunc ClientEnv
intrayTestClientEnvSetupFunc' menv man pool = do
  signingKey <- liftIO Auth.generateKey
  let jwtCfg = defaultJWTSettings signingKey
  let cookieCfg = defaultCookieSettings
  logFunc <- runNoLoggingT askLoggerIO
  let intrayEnv =
        IntrayServerEnv
          { envLogFunc = logFunc,
            envHost = "localhost",
            envConnectionPool = pool,
            envCookieSettings = cookieCfg,
            envJWTSettings = jwtCfg,
            envAdmins = [fromJust $ parseUsername "admin"],
            envFreeloaders = [],
            envMonetisation = menv
          }
  let application = serveWithContext intrayAPI (intrayAppContext intrayEnv) (makeIntrayServer intrayEnv)
  p <- applicationSetupFunc application
  pure $ mkClientEnv man (BaseUrl Http "127.0.0.1" (fromIntegral p) "")

runClient :: ClientEnv -> ClientM a -> IO (Either ClientError a)
runClient = flip runClientM

runClientOrError :: ClientEnv -> ClientM a -> IO a
runClientOrError cenv func = do
  errOrRes <- runClient cenv func
  case errOrRes of
    Left err -> expectationFailure $ show err
    Right res -> pure res

withAdmin :: ClientEnv -> (Token -> IO ()) -> Expectation
withAdmin cenv = withNewUser cenv (Registration (fromJust $ parseUsername "admin") "admin")

withValidNewUser :: ClientEnv -> (Token -> IO ()) -> Expectation
withValidNewUser cenv func = withValidNewUserAndData cenv $ \_ _ -> func

randomRegistration :: IO Registration
randomRegistration = do
  u1 <- nextRandomUUID :: IO (UUID Username) -- Dummy's that are significantly likely to be random enough
  u2 <- nextRandomUUID :: IO (UUID Text)
  pure
    Registration
      { registrationUsername = fromJust $ parseUsername $ uuidText u1,
        registrationPassword = uuidText u2
      }

withValidNewUserAndData :: ClientEnv -> (Username -> Text -> Token -> IO ()) -> Expectation
withValidNewUserAndData cenv func = do
  r <- randomRegistration
  withNewUser cenv r $ func (registrationUsername r) (registrationPassword r)

withNewUser :: ClientEnv -> Registration -> (Token -> IO ()) -> Expectation
withNewUser cenv r func = do
  errOrUUID <- runClient cenv $ clientPostRegister r
  case errOrUUID of
    Left err -> expectationFailure $ "Registration should not fail with error: " <> show err
    Right NoContent -> login cenv (registrationUsername r) (registrationPassword r) >>= func

requiresAdmin :: ClientEnv -> (Token -> ClientM a) -> Expectation
requiresAdmin cenv func =
  withValidNewUser cenv $ \token -> do
    errOrStats <- runClient cenv $ func token
    case errOrStats of
      Left err ->
        case err of
          FailureResponse _ resp ->
            HTTP.statusCode (Servant.Client.responseStatusCode resp) `shouldBe` 401
          _ -> expectationFailure "Should have got a expectationFailure response."
      Right _ -> expectationFailure "Should not have been allowed."

withNewUser'sAccessKey :: ClientEnv -> Set Permission -> (Token -> IO ()) -> Expectation
withNewUser'sAccessKey cenv ps func =
  withValidNewUserAndData cenv $ \un _ tok -> do
    uuid <- nextRandomUUID :: IO (UUID AddAccessKey) -- Dummy's that are significantly likely to be random enough
    let aac = AddAccessKey {addAccessKeyName = uuidText uuid, addAccessKeyPermissions = ps}
    errOrAkc <- runClient cenv $ clientPostAddAccessKey tok aac
    case errOrAkc of
      Left err -> expectationFailure $ unwords ["Failed to create access key:", show err]
      Right AccessKeyCreated {..} -> do
        t <- login cenv un (accessKeySecretText accessKeyCreatedKey)
        func t

login :: ClientEnv -> Username -> Text -> IO Token
login cenv un pw = do
  let lf = LoginForm {loginFormUsername = un, loginFormPassword = pw}
  Headers NoContent (Servant.HCons sessionHeader Servant.HNil) <- runClientOrError cenv $ clientPostLogin lf
  case sessionHeader of
    MissingHeader -> expectationFailure "Login should return a session header"
    UndecodableHeader _ -> expectationFailure "Login should return a decodable session header"
    Header session -> pure $ Token $ setCookieValue $ parseSetCookie $ encodeUtf8 session

failsWithOutPermissions :: ClientEnv -> Set Permission -> (Token -> ClientM a) -> Property
failsWithOutPermissions cenv ps func =
  forAll (genValid `suchThat` (\ps' -> null (ps' `S.intersection` ps))) $ \perms ->
    withNewUser'sAccessKey cenv (perms `S.intersection` userPermissions) $ \t -> do
      res <- runClient cenv $ func t
      case res of
        Left err ->
          case err of
            FailureResponse _ resp ->
              HTTP.statusCode (Servant.Client.responseStatusCode resp) `shouldBe` 401
            _ -> expectationFailure "Should have gotten a expectationFailure response."
        _ -> expectationFailure "Should not have been allowed."

failsWithOutPermission :: ClientEnv -> Permission -> (Token -> ClientM a) -> Property
failsWithOutPermission cenv p = failsWithOutPermissions cenv $ S.singleton p
