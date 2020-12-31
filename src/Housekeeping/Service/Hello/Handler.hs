{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

module Housekeeping.Service.Hello.Handler
  ( helloHandlerImpl,
    HelloRepository (..),
    HasHelloRepository (..),
  )
where

import Housekeeping.Callable (call)
import Housekeeping.Service.Hello.Interface
  ( HasHelloRepository (..),
    HelloHandler (..),
    HelloRepository (..),
  )
import Housekeeping.Service.Hello.Model (Hello (..))
import RIO
import Servant.Server

helloHandlerImpl :: (HasLogFunc env, HasHelloRepository env) => HelloHandler env
helloHandlerImpl =
  HelloHandler
    { helloHandler = helloImpl,
      worldHandler = worldImpl,
      errorHandler = errorImpl,
      fatalHandler = fatalImpl,
      selectHandler = selectImpl,
      insertHandler = insertImpl
    }

insertImpl :: (HasCallStack, HasLogFunc env, HasHelloRepository env) => Text -> RIO env ()
insertImpl msg = do
  logInfo $ "insert message: " <> display msg
  call (helloRepositoryL . to insertMessage) msg

selectImpl :: (HasCallStack, HasLogFunc env, HasHelloRepository env) => RIO env [Text]
selectImpl = do
  logInfo "select message"
  call (helloRepositoryL . to selectMessage)

helloImpl :: (HasCallStack, HasLogFunc env) => RIO env Hello
helloImpl = do
  logInfo "GET Hello"
  pure Hello

worldImpl :: (HasCallStack, HasLogFunc env) => RIO env Hello
worldImpl = do
  logInfo "GET World"
  pure World

errorImpl :: (HasCallStack, HasLogFunc env) => RIO env ()
errorImpl = do
  logInfo "GET Error"
  throwIO err400

fatalImpl :: (HasCallStack, HasLogFunc env) => RIO env ()
fatalImpl = do
  logInfo "GET Fatal"
  undefined
