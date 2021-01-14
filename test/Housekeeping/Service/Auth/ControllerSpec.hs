{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}

module Housekeeping.Service.Auth.ControllerSpec where

import Control.Monad.Except (ExceptT (ExceptT))
import Housekeeping.Service.Auth.Controller
  ( AuthResponse,
    PasswordForm (PasswordForm),
    api,
    server,
  )
import Housekeeping.Service.Auth.Interface
  ( AuthConfig (..),
    AuthHandler (..),
    HasAuthConfig (..),
    HasAuthHandler (..),
    cookieSettings,
    jwtSettings,
  )
import Housekeeping.Service.Auth.Model
  ( PlainPassword (PlainPassword),
    User (User),
  )
import Lens.Micro.Platform (makeLenses, (^.))
import Network.HTTP.Client (defaultManagerSettings, newManager)
import Network.HTTP.Types (Status (statusCode))
import qualified Network.Wai.Handler.Warp as Warp
import RIO (RIO, catch, runRIO)
import RIO.Prelude.Types (Proxy (Proxy))
import Servant (Headers (getResponse), JSON, Post, ReqBody, (:>))
import Servant.Auth.Server
  ( AuthResult (Authenticated, NoSuchUser),
    CookieSettings,
    JWTSettings,
    defaultCookieSettings,
    defaultJWTSettings,
    generateKey,
  )
import Servant.Client (BaseUrl (baseUrlPort), ClientError (FailureResponse), ResponseF (responseStatusCode), client, parseBaseUrl, runClientM)
import Servant.Client.Internal.HttpClient (mkClientEnv)
import Servant.Server
  ( Application,
    Context (EmptyContext, (:.)),
    Handler (..),
    HasServer (hoistServerWithContext),
    serveWithContext,
  )
import Test.Hspec
  ( Selector,
    Spec,
    around,
    context,
    describe,
    it,
    runIO,
    shouldBe,
    shouldSatisfy,
  )
import Test.Method
  ( ArgsMatcher (args),
    anything,
    mockup,
    thenMethod,
    thenReturn,
    when,
  )
import Crypto.JOSE (JWK)

mockAuthHandler :: AuthHandler env
mockAuthHandler =
  AuthHandler
    { _signinHandler = mockup $ do
        when (args ((== "user1"), (== PlainPassword "password1"))) `thenReturn` Authenticated (User "user1")
        when anything `thenReturn` NoSuchUser,
      _signupHandler = mockup $ do
        when (args ((== "user1"), anything)) `thenReturn` Nothing
        when anything `thenMethod` (\usernm !_ -> pure $ Just $ User usernm)
    }

mockAuthConfig :: JWK -> AuthConfig
mockAuthConfig jwk =
  AuthConfig
    { _jwtSettings = defaultJWTSettings jwk,
      _cookieSettings = defaultCookieSettings
    }

data Env = Env
  { _authHandler :: AuthHandler Env,
    _authConfig :: AuthConfig
  }

makeLenses ''Env

instance HasAuthConfig Env where
  authConfigL = authConfig

instance HasAuthHandler Env where
  authHandlerL = authHandler

mkEnv :: JWK -> Env
mkEnv jwk = Env mockAuthHandler (mockAuthConfig jwk)

testApp :: Env -> Application
testApp env = serveWithContext api ctx $ hoistServerWithContext api ctxProxy nt server
  where
    nt :: RIO Env a -> Handler a
    nt action =
      Handler $
        ExceptT $
          (Right <$> runRIO env action)
            `catch` (pure . Left)
    cookie = env ^. authConfigL . cookieSettings
    jwt = env ^. authConfigL . jwtSettings
    ctx = cookie :. jwt :. EmptyContext
    ctxProxy :: Proxy '[CookieSettings, JWTSettings]
    ctxProxy = Proxy

withTestApp :: (Warp.Port -> IO ()) -> IO ()
withTestApp =
  Warp.testWithApplication $ testApp . mkEnv <$> generateKey

signinApi :: Proxy ("signin" :> ReqBody '[JSON] PasswordForm :> Post '[JSON] (AuthResponse User))
signinApi = Proxy

signupApi :: Proxy ("signup" :> ReqBody '[JSON] PasswordForm :> Post '[JSON] User)
signupApi = Proxy

spec :: Spec
spec = around withTestApp $ do
  baseUrl <- runIO $ parseBaseUrl "http://localhost"
  manager <- runIO $ newManager defaultManagerSettings
  let clientEnv port = mkClientEnv manager baseUrl {baseUrlPort = port}
  describe "POST /signin" $ do
    context "if correct userId and password is given" $ do
      it "should return User" $ \port -> do
        let form = PasswordForm "user1" "password1"
        result <- runClientM (client signinApi form) (clientEnv port)
        fmap getResponse result `shouldBe` Right (User "user1")

    context "if incorrect password is given" $ do
      it "should return unauthorized response" $ \port -> do
        let form = PasswordForm "user1" "password2"
        result <- runClientM (client signinApi form) (clientEnv port)
        fmap getResponse result `shouldSatisfy` anyErrorStatus 401

    context "if password contains non-ascii chars" $ do
      it "should return client error response" $ \port -> do
        let form = PasswordForm "user1" "パスワード"
        result <- runClientM (client signinApi form) (clientEnv port)
        fmap getResponse result `shouldSatisfy` anyErrorStatus 400

    context "if unregistered user is given" $ do
      it "should return unauthorized response" $ \port -> do
        let form = PasswordForm "user2" "password2"
        result <- runClientM (client signinApi form) (clientEnv port)
        fmap getResponse result `shouldSatisfy` anyErrorStatus 401

  describe "signup" $ do
    context "if username is not taken" $ do
      it "should return user" $ \port -> do
        let form = PasswordForm "user2" "password2"
        result <- runClientM (client signupApi form) (clientEnv port)
        result `shouldBe` Right (User "user2")
    context "if username is taken" $ do
      it "should return 409 error response" $ \port -> do
        let form = PasswordForm "user1" "password1"
        result <- runClientM (client signupApi form) (clientEnv port)
        result `shouldSatisfy` anyErrorStatus 409
    context "if password contains non-ascii characters" $ do
      it "should return 400 error response" $ \port -> do
        let form = PasswordForm "user2" "パスワード"
        result <- runClientM (client signupApi form) (clientEnv port)
        result `shouldSatisfy` anyErrorStatus 400

anyErrorStatus :: Int -> Selector (Either ClientError a)
anyErrorStatus code (Left (FailureResponse _ res)) =
  statusCode (responseStatusCode res) == code
anyErrorStatus _ _ = False
