{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}

module Housekeeping.Service.Auth.ControllerSpec where

import Control.Monad.Except (ExceptT (ExceptT))
import Crypto.JOSE.JWK
  ( JWK,
    KeyMaterialGenParam (RSAGenParam),
    genJWK,
  )
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
  ( Spec,
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
    thenReturn,
    when,
  )

mockAuthHandler :: AuthHandler env
mockAuthHandler =
  AuthHandler
    { _signinHandler = mockup $ do
        when (args ((== "user1"), (== PlainPassword "password1"))) `thenReturn` Authenticated (User "user1")
        when anything `thenReturn` NoSuchUser,
      _signupHandler =
        mockup $ pure ()
    }

genKey :: IO JWK
genKey = genJWK (RSAGenParam (4096 `div` 8))

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
  Warp.testWithApplication $ testApp . mkEnv <$> genKey

signinApi :: Proxy ("signin" :> ReqBody '[JSON] PasswordForm :> Post '[JSON] (AuthResponse User))
signinApi = Proxy

spec :: Spec
spec = around withTestApp $ do
  baseUrl <- runIO $ parseBaseUrl "http://localhost"
  manager <- runIO $ newManager defaultManagerSettings
  let clientEnv port = mkClientEnv manager baseUrl {baseUrlPort = port}
  describe "POST /signin" $ do
    context "if correct userId and password is given" $ do
      it "should return User" $ \port -> do
        let form = PasswordForm "user1" "password1"
        result <- runClientM (client api form) (clientEnv port)
        fmap getResponse result `shouldBe` Right (User "user1")

    context "if incorrect password is given" $ do
      it "should return unauthorized response" $ \port -> do
        let form = PasswordForm "user1" "password2"
        result <- runClientM (client api form) (clientEnv port)
        fmap getResponse result `shouldSatisfy` \case
          Left (FailureResponse _ res) ->
            statusCode (responseStatusCode res) == 401
          _ -> False

    context "if password contains non-ascii chars" $ do
      it "should return client error response" $ \port -> do
        let form = PasswordForm "user1" "パスワード"
        result <- runClientM (client api form) (clientEnv port)
        fmap getResponse result `shouldSatisfy` \case
          Left (FailureResponse _ res) ->
            statusCode (responseStatusCode res) == 400
          _ -> False

    context "if unregistered user is given" $ do
      it "should return unauthorized response" $ \port -> do
        let form = PasswordForm "user2" "password2"
        result <- runClientM (client api form) (clientEnv port)
        fmap getResponse result `shouldSatisfy` \case
          Left (FailureResponse _ res) ->
            statusCode (responseStatusCode res) == 401
          _ -> False