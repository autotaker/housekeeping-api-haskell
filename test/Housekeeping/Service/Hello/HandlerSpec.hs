{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Housekeeping.Service.Hello.HandlerSpec where

import Housekeeping.Service.Hello.Handler
import Housekeeping.Service.Hello.Interface
import Housekeeping.Service.Hello.Model
import Lens.Micro.Platform
import RIO
import Servant.Server
import Test.Hspec
import Test.Method

data MockEnv = MockEnv
  { _logFunc :: LogFunc,
    _helloRepository :: HelloRepository MockEnv
  }

makeLenses ''MockEnv

instance HasLogFunc MockEnv where
  logFuncL = logFunc

instance HasHelloRepository MockEnv where
  helloRepositoryL = helloRepository

run :: RIO MockEnv a -> IO a
run action = do
  options <- logOptionsHandle stderr True
  withLogFunc options $ \lf -> do
    runRIO (mockEnv lf) action

mockEnv :: LogFunc -> MockEnv
mockEnv lf =
  MockEnv
    { _logFunc = lf,
      _helloRepository =
        HelloRepository
          { _selectMessage = pure ["Hello World!"],
            _insertMessage = \_ -> pure ()
          }
    }

spec :: Spec
spec = do
  let HelloHandler {..} = helloHandlerImpl
  describe "helloHandler" $
    it "return Hello" $ do
      run _helloHandler `shouldReturn` Hello

  describe "worldHandler" $
    it "return World" $ do
      run _worldHandler `shouldReturn` World

  describe "errorHandler" $
    it "should throw 400 error" $ do
      let any400Error err = errHTTPCode err == 400
      run _errorHandler `shouldThrow` any400Error

  describe "fatalHandler" $
    it "should throw undefined" $ do
      run _fatalHandler `shouldThrow` anyException

  describe "selectHandler" $
    it "should call selectMessage" $ do
      run _selectHandler `shouldReturn` ["Hello World!"]

  describe "insertHandler" $
    it "should call insertMessage" $ do
      logs <- run $
        withMonitor_ $ \monitor ->
          local (helloRepositoryL . insertMessage %~ watch monitor) $
            _insertHandler "INSERT TEST"
      logs `shouldSatisfy` (== 1) `times` call (args (== "INSERT TEST"))
