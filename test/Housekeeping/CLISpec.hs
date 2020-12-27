{-# LANGUAGE OverloadedStrings #-}

module Housekeeping.CLISpec where

import Housekeeping.CLI
import Network.HTTP.Types.Method
import Network.HTTP.Types.Status
import Network.Wai
import Network.Wai.Internal
import RIO
import qualified RIO.Text as T
import Test.Hspec

spec :: Spec
spec = do
  describe "formatAccessLog" $ do
    it "contains http method" $ do
      let req = defaultRequest {requestMethod = methodGet}
          status = status200
          mlen = Nothing
      textDisplay (formatAccessLog req status mlen)
        `shouldSatisfy` T.isInfixOf "method:GET"

    it "contains http path" $ do
      let req = defaultRequest {rawPathInfo = "/hoge/fuga"}
          status = status200
          mlen = Nothing
      textDisplay (formatAccessLog req status mlen)
        `shouldSatisfy` T.isInfixOf "path:/hoge/fuga"

    it "contains status code" $ do
      let req = defaultRequest
          status = status200
          mlen = Nothing
      textDisplay (formatAccessLog req status mlen)
        `shouldSatisfy` T.isInfixOf "code:200"

    it "contains length" $ do
      let req = defaultRequest
          status = status200
          mlen = Just 42
      textDisplay (formatAccessLog req status mlen)
        `shouldSatisfy` T.isInfixOf "length:42"

    it "contains length:-" $ do
      let req = defaultRequest
          status = status200
          mlen = Nothing
      textDisplay (formatAccessLog req status mlen)
        `shouldSatisfy` T.isInfixOf "length:-"
