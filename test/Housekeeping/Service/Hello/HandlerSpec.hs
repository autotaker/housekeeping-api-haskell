{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Housekeeping.Service.Hello.HandlerSpec where

import Control.Env.Hierarchical
import Housekeeping.Service.Hello.Handler
import Housekeeping.Service.Hello.Interface
import Housekeeping.Service.Hello.Model
import RIO
import Servant.Server
import Test.Hspec
import Test.Method

data MockEnv = MockEnv LogFunc (HelloRepository MockEnv)

deriveEnv ''MockEnv

deriveLabel ''HelloRepository

runP :: ProtocolM (HelloRepositoryLabel MockEnv) b -> RIO MockEnv a -> IO a
runP proto action =
  withProtocol proto $ \repo -> do
    (_logger, options) <- logOptionsMemory
    withLogFunc options $ \lf -> do
      runRIO (MockEnv lf repo) action

spec :: Spec
spec = do
  let HelloHandler {..} = helloHandlerImpl
  describe "helloHandler" $
    it "return Hello" $ do
      runP (pure ()) helloHandler `shouldReturn` Hello

  describe "worldHandler" $
    it "return World" $ do
      runP (pure ()) worldHandler `shouldReturn` World

  describe "errorHandler" $
    it "should throw 400 error" $ do
      let any400Error err = errHTTPCode err == 400
      runP (pure ()) errorHandler `shouldThrow` any400Error

  describe "fatalHandler" $
    it "should throw undefined" $ do
      runP (pure ()) fatalHandler `shouldThrow` anyException

  describe "selectHandler" $
    it "should call selectMessage" $ do
      let proto =
            decl $ whenArgs SelectMessage () `thenReturn` ["Hello World!"]
      runP proto selectHandler `shouldReturn` ["Hello World!"]

  describe "insertHandler" $
    it "should call insertMessage" $ do
      let proto =
            decl $ whenArgs InsertMessage (== "INSERT TEST") `thenReturn` ()
      runP proto (insertHandler "INSERT TEST")
