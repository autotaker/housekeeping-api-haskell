module Housekeeping.HandlerSpec where

import Housekeeping.Handler
import Housekeeping.Model
import RIO
import Servant.Server
import Test.Hspec

spec :: Spec
spec = do
  describe "helloHandler" $
    it "return Hello" $ do
      runSimpleApp helloHandler `shouldReturn` Hello

  describe "worldHandler" $
    it "return World" $ do
      runSimpleApp worldHandler `shouldReturn` World

  describe "errorHandler" $
    it "should throw 400 error" $ do
      let any400Error err = errHTTPCode err == 400
      runSimpleApp errorHandler `shouldThrow` any400Error

  describe "fatalHandler" $
    it "should throw undefined" $ do
      runSimpleApp fatalHandler `shouldThrow` anyException
