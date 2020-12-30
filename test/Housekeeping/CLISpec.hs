{-# LANGUAGE OverloadedStrings #-}

module Housekeeping.CLISpec where

import Housekeeping.CLI
import Network.HTTP.Types.Method
import Network.HTTP.Types.Status
import Network.Wai
import RIO
import qualified RIO.Text as T
import Test.Hspec

spec :: Spec
spec = do
  let req = defaultRequest
      status = status200
      mlen = Nothing

  describe "formatAccessLog" $ do
    it "contains http method" $ do
      let req' = req {requestMethod = methodGet}
      textDisplay (formatAccessLog req' status mlen)
        `shouldSatisfy` T.isInfixOf "method:GET"

    it "contains http path" $ do
      let req' = req {rawPathInfo = "/hoge/fuga"}
      textDisplay (formatAccessLog req' status mlen)
        `shouldSatisfy` T.isInfixOf "path:/hoge/fuga"

    it "contains status code" $ do
      textDisplay (formatAccessLog req status404 mlen)
        `shouldSatisfy` T.isInfixOf "code:404"

    it "contains length" $ do
      textDisplay (formatAccessLog req status (Just 42))
        `shouldSatisfy` T.isInfixOf "length:42"

    it "contains length:-" $ do
      textDisplay (formatAccessLog req status mlen)
        `shouldSatisfy` T.isInfixOf "length:-"
