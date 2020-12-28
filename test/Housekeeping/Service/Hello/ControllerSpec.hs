{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators #-}

module Housekeeping.Service.Hello.ControllerSpec where

import Control.Monad.Except
import Housekeeping.Service.Hello.Controller
import Housekeeping.Service.Hello.Model
import Network.HTTP.Client (defaultManagerSettings, newManager)
import Network.HTTP.Types
import qualified Network.Wai.Handler.Warp as Warp
import RIO ()
import Servant
import Servant.Client
import Servant.Server
import Test.Hspec

newtype HelloControllerMock a = HelloControllerMock {unHelloControllerMock :: Handler a}

deriving instance Functor HelloControllerMock

deriving instance Applicative HelloControllerMock

deriving instance Monad HelloControllerMock

deriving instance MonadError ServerError HelloControllerMock

instance HelloController HelloControllerMock where
  helloHandler = pure Hello
  worldHandler = pure World
  errorHandler = throwError err400
  fatalHandler = throwError err500

testApp :: Application
testApp = serve api $ hoistServer api unHelloControllerMock server

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

spec :: Spec
spec = do
  around withTestApp $ do
    let helloClient = client api
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