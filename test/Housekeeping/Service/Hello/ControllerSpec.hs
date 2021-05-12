{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Housekeeping.Service.Hello.ControllerSpec where

import Control.Env.Hierarchical
import Control.Monad.Except
import Housekeeping.Service.Hello.Controller
import Housekeeping.Service.Hello.Interface
import Housekeeping.Service.Hello.Model
import Housekeeping.Session
import Network.HTTP.Client (defaultManagerSettings, newManager)
import Network.HTTP.Types
import qualified Network.Wai.Handler.Warp as Warp
import RIO (Display (textDisplay), RIO, Text, catch, runRIO, throwIO, (^.))
import qualified RIO.ByteString.Lazy as BS
import Servant
import Servant.Auth.Client
import Servant.Auth.Server
import Servant.Client (BaseUrl (baseUrlPort), ClientError (FailureResponse), Response, ResponseF (responseStatusCode), client, mkClientEnv, parseBaseUrl, runClientM)
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

mockHelloHandler :: HelloHandler env
mockHelloHandler =
  HelloHandler
    { helloHandler = pure Hello,
      worldHandler = pure World,
      errorHandler = throwIO err400,
      fatalHandler = throwIO err500,
      selectHandler = pure ["MESSAGE"],
      insertHandler = \x -> liftIO $ x `shouldBe` "INSERT TEST",
      secretHandler = \user ->
        pure $ Secret $ textDisplay $ user ^. userName
    }

newtype Env = Env (HelloHandler Env)

deriveEnv ''Env

testApp :: SessionConfig -> Application
testApp config = serveWithContext api ctx $ hoistServerWithContext api ctxProxy nt server
  where
    ctx = (config ^. jwtSettings) :. (config ^. cookieSettings) :. EmptyContext
    ctxProxy :: Proxy '[JWTSettings, CookieSettings]
    ctxProxy = Proxy
    nt :: RIO Env a -> Handler a
    nt action =
      Handler $
        ExceptT $
          (Right <$> runRIO (Env mockHelloHandler) action)
            `catch` (pure . Left)

withTestApp :: SessionConfig -> (Warp.Port -> IO ()) -> IO ()
withTestApp config =
  Warp.testWithApplication $ pure $ testApp config

helloAPI :: Proxy ("hello" :> Get '[JSON] Hello)
helloAPI = Proxy

worldAPI :: Proxy ("world" :> Get '[JSON] Hello)
worldAPI = Proxy

errorAPI :: Proxy ("error" :> Get '[JSON] ())
errorAPI = Proxy

fatalAPI :: Proxy ("fatal" :> Get '[JSON] ())
fatalAPI = Proxy

selectAPI :: Proxy ("message" :> Get '[JSON] [Text])
selectAPI = Proxy

insertAPIForm :: Proxy ("message" :> ReqBody '[FormUrlEncoded] MessageForm :> Post '[JSON] ())
insertAPIForm = Proxy

insertAPIJSON :: Proxy ("message" :> ReqBody '[JSON] MessageForm :> Post '[JSON] ())
insertAPIJSON = Proxy

secretAPI :: Proxy ("secret" :> Auth '[JWT] User :> Get '[JSON] Hello)
secretAPI = Proxy

secretAPIUnauthed :: Proxy ("secret" :> Get '[JSON] Hello)
secretAPIUnauthed = Proxy

isFailure :: (Response -> Bool) -> Either ClientError a -> Bool
isFailure matcher (Left (FailureResponse _ res)) = matcher res
isFailure _ _ = False

spec :: Spec
spec = do
  config <- runIO defaultSessionConfig
  around (withTestApp config) $ do
    baseUrl <- runIO $ parseBaseUrl "http://localhost"
    manager <- runIO $ newManager defaultManagerSettings
    let clientEnv port = mkClientEnv manager baseUrl {baseUrlPort = port}

    describe "GET /hello" $ do
      it "should return Hello" $ \port -> do
        result <- runClientM (client helloAPI) (clientEnv port)
        result `shouldBe` Right Hello

    describe "GET /world" $ do
      it "should return World" $ \port -> do
        result <- runClientM (client worldAPI) (clientEnv port)
        result `shouldBe` Right World

    describe "GET /error" $ do
      it "should return client error response" $ \port -> do
        result <- runClientM (client errorAPI) (clientEnv port)
        result `shouldSatisfy` isFailure (statusIsClientError . responseStatusCode)

    describe "GET /fatal" $ do
      it "should return server error response" $ \port -> do
        result <- runClientM (client fatalAPI) (clientEnv port)
        result `shouldSatisfy` isFailure (statusIsServerError . responseStatusCode)

    describe "GET /message" $ do
      it "should return list of messages" $ \port -> do
        result <- runClientM (client selectAPI) (clientEnv port)
        result `shouldBe` Right ["MESSAGE"]

    describe "POST /message" $ do
      it "should accept message in x-www-form-urlencoded form" $ \port -> do
        let form = MessageForm "INSERT TEST"
        result <- runClientM (client insertAPIForm form) (clientEnv port)
        result `shouldBe` Right ()

      it "should accept message in json form" $ \port -> do
        let form = MessageForm "INSERT TEST"
        result <- runClientM (client insertAPIJSON form) (clientEnv port)
        result `shouldBe` Right ()
    describe "GET /secret" $ do
      context "if session is authenticated" $
        it "should return user name of the current session" $ \port -> do
          let user = User 1 "user1"
          Right jwt <- makeJWT user (config ^. jwtSettings) Nothing
          let token = Token $ BS.toStrict jwt
          result <- runClientM (client secretAPI token) (clientEnv port)
          result `shouldBe` Right (Secret "user1")
      context "if session is not authenticated" $
        it "should return user name of the current session" $ \port -> do
          result <- runClientM (client secretAPIUnauthed) (clientEnv port)
          result `shouldSatisfy` isFailure ((== 401) . statusCode . responseStatusCode)
