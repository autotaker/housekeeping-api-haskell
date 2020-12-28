module Housekeeping.Service.Hello.HandlerSpec where

import Housekeeping.Service.Hello.Controller
import Housekeeping.Service.Hello.Handler
import Housekeeping.Service.Hello.Model
import RIO
import Servant.Server
import Test.Hspec

run :: HelloControllerImpl SimpleApp a -> IO a
run = runHelloControllerSimpleApp

spec :: Spec
spec = do
  describe "helloHandler" $
    it "return Hello" $ do
      run helloHandler `shouldReturn` Hello

  describe "worldHandler" $
    it "return World" $ do
      run worldHandler `shouldReturn` World

  describe "errorHandler" $
    it "should throw 400 error" $ do
      let any400Error err = errHTTPCode err == 400
      run errorHandler `shouldThrow` any400Error

  describe "fatalHandler" $
    it "should throw undefined" $ do
      run fatalHandler `shouldThrow` anyException
