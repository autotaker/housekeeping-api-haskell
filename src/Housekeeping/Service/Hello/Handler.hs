{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

module Housekeeping.Service.Hello.Handler
  ( helloControllerImpl,
    HelloRepository (..),
    HasHelloRepository (..),
  )
where

import Housekeeping.Service.Hello.Controller (HelloController (..))
import Housekeeping.Service.Hello.Model (Hello (..))
import RIO
import Servant.Server

helloControllerImpl :: (HasLogFunc env, HasHelloRepository env) => HelloController env
helloControllerImpl =
  HelloController
    { helloHandler = helloHandlerImpl,
      worldHandler = worldHandlerImpl,
      errorHandler = errorHandlerImpl,
      fatalHandler = fatalHandlerImpl,
      selectHandler = selectHandlerImpl,
      insertHandler = insertHandlerImpl
    }

class HasHelloRepository env where
  helloRepositoryL :: Lens' env (HelloRepository env)

data HelloRepository env = HelloRepository
  { insertMessage :: Text -> RIO env (),
    selectMessage :: RIO env [Text]
  }

insertHandlerImpl :: (HasCallStack, HasLogFunc env, HasHelloRepository env) => Text -> RIO env ()
insertHandlerImpl msg = do
  logInfo $ "insert message: " <> display msg
  method <- view $ helloRepositoryL . to insertMessage
  method msg

selectHandlerImpl :: (HasCallStack, HasLogFunc env, HasHelloRepository env) => RIO env [Text]
selectHandlerImpl = do
  logInfo "select message"
  join $ view $ helloRepositoryL . to selectMessage

helloHandlerImpl :: (HasCallStack, HasLogFunc env) => RIO env Hello
helloHandlerImpl = do
  logInfo "GET Hello"
  pure Hello

worldHandlerImpl :: (HasCallStack, HasLogFunc env) => RIO env Hello
worldHandlerImpl = do
  logInfo "GET World"
  pure World

errorHandlerImpl :: (HasCallStack, HasLogFunc env) => RIO env ()
errorHandlerImpl = do
  logInfo "GET Error"
  throwIO err400

fatalHandlerImpl :: (HasCallStack, HasLogFunc env) => RIO env ()
fatalHandlerImpl = do
  logInfo "GET Fatal"
  undefined
