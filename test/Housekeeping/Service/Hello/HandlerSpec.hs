{-# LANGUAGE OverloadedStrings #-}

module Housekeeping.Service.Hello.HandlerSpec where

import Housekeeping.Service.Hello.Controller
import Housekeeping.Service.Hello.Handler
import Housekeeping.Service.Hello.Model
import RIO
import Servant.Server
import Test.Hspec

data MockEnv = MockEnv
  { logFunc :: LogFunc,
    helloRepository :: HelloRepository MockEnv
  }

instance HasLogFunc MockEnv where
  logFuncL = lens logFunc (\x y -> x {logFunc = y})

instance HasHelloRepository MockEnv where
  helloRepositoryL = lens helloRepository (\x y -> x {helloRepository = y})

run :: RIO MockEnv a -> IO a
run action = do
  options <- logOptionsHandle stderr True
  withLogFunc options $ \lf -> do
    let mockEnv =
          MockEnv
            { logFunc = lf,
              helloRepository =
                HelloRepository
                  { selectMessage = selectMessageMock,
                    insertMessage = insertMessageMock
                  }
            }
    runRIO mockEnv action

selectMessageMock :: RIO MockEnv [Text]
selectMessageMock = pure ["Hello World!"]

insertMessageMock :: (MonadIO m, Show a, Eq a, IsString a) => a -> m ()
insertMessageMock x =
  liftIO $ x `shouldBe` "INSERT TEST"

spec :: Spec
spec = do
  describe "helloHandler" $
    it "return Hello" $ do
      run (helloHandler helloControllerImpl) `shouldReturn` Hello

  describe "worldHandler" $
    it "return World" $ do
      run (worldHandler helloControllerImpl) `shouldReturn` World

  describe "errorHandler" $
    it "should throw 400 error" $ do
      let any400Error err = errHTTPCode err == 400
      run (errorHandler helloControllerImpl) `shouldThrow` any400Error

  describe "fatalHandler" $
    it "should throw undefined" $ do
      run (fatalHandler helloControllerImpl) `shouldThrow` anyException

  describe "selectHandler" $
    it "should call selectMessage" $ do
      run (selectHandler helloControllerImpl) `shouldReturn` ["Hello World!"]

  describe "insertHandler" $
    it "should call insertMessage" $ do
      run (insertHandler helloControllerImpl "INSERT TEST") `shouldReturn` ()
