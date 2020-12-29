{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Housekeeping.Service.Hello.ControllerSpec where

import Control.Monad.Except
import Housekeeping.Service.Hello.Controller
import Housekeeping.Service.Hello.Model
import Network.HTTP.Client (defaultManagerSettings, newManager)
import Network.HTTP.Types
import qualified Network.Wai.Handler.Warp as Warp
import RIO (RIO, Text, catch, runRIO, throwIO)
import Servant
import Servant.Client
  ( BaseUrl (baseUrlPort),
    ClientError (FailureResponse),
    ResponseF (responseStatusCode),
    client,
    mkClientEnv,
    parseBaseUrl,
    runClientM,
  )
import Test.Hspec

mockHelloController :: HelloController env
mockHelloController =
  HelloController
    { helloHandler = pure Hello,
      worldHandler = pure World,
      errorHandler = throwIO err400,
      fatalHandler = throwIO err500,
      selectHandler = pure ["MESSAGE"],
      insertHandler = \x -> liftIO $ x `shouldBe` "INSERT TEST"
    }

testApp :: Application
testApp = serve api $ hoistServer api nt (server mockHelloController)
  where
    nt :: RIO () a -> Handler a
    nt action =
      Handler $
        ExceptT $
          (Right <$> runRIO () action)
            `catch` (pure . Left)

withTestApp :: (Warp.Port -> IO ()) -> IO ()
withTestApp = Warp.testWithApplication (pure testApp)

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

spec :: Spec
spec = do
  around withTestApp $ do
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
        result `shouldSatisfy` \case
          Left (FailureResponse _ res) -> statusIsClientError (responseStatusCode res)
          _ -> False

    describe "GET /fatal" $ do
      it "should return server error response" $ \port -> do
        result <- runClientM (client fatalAPI) (clientEnv port)
        result `shouldSatisfy` \case
          Left (FailureResponse _ res) -> statusIsServerError (responseStatusCode res)
          _ -> False

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
