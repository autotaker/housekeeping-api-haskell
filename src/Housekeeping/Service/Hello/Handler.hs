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

import Control.Method
import Housekeeping.Service.Hello.Interface
  ( HasHelloRepository (..),
    HelloHandler (..),
    HelloRepository (..),
    ViewHelloRepository (..),
    insertMessage,
    selectMessage,
  )
import Housekeeping.Service.Hello.Model (Hello (..))
import RIO
import Servant.Server

helloHandlerImpl :: (HasLogFunc env, ViewHelloRepository env) => HelloHandler env
helloHandlerImpl =
  HelloHandler
    { _helloHandler = helloImpl,
      _worldHandler = worldImpl,
      _errorHandler = errorImpl,
      _fatalHandler = fatalImpl,
      _selectHandler = selectImpl,
      _insertHandler = insertImpl
    }

insertImpl :: (HasCallStack, HasLogFunc env, ViewHelloRepository env) => Text -> RIO env ()
insertImpl msg = do
  logInfo $ "insert message: " <> display msg
  invoke (helloRepositoryV . insertMessage) msg

selectImpl :: (HasCallStack, HasLogFunc env, ViewHelloRepository env) => RIO env [Text]
selectImpl = do
  logInfo "select message"
  invoke (helloRepositoryV . selectMessage)

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
