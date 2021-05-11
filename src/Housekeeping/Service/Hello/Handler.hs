{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Housekeeping.Service.Hello.Handler
  ( helloHandlerImpl,
    HelloRepository (..),
  )
where

import Control.Env.Hierarchical
import Housekeeping.Prelude
import Housekeeping.Service.Hello.Interface
  ( HelloHandler (..),
    HelloRepository (..),
    insertMessage,
    selectMessage,
  )
import Housekeeping.Service.Hello.Model (Hello (..))
import Servant.Server

helloHandlerImpl :: (Has LogFunc env, Has1 HelloRepository env) => HelloHandler env
helloHandlerImpl =
  HelloHandler
    { helloHandler = helloImpl,
      worldHandler = worldImpl,
      errorHandler = errorImpl,
      fatalHandler = fatalImpl,
      selectHandler = selectImpl,
      insertHandler = insertImpl
    }

insertImpl :: (Has LogFunc env, Has1 HelloRepository env) => Text -> RIO env ()
insertImpl msg = do
  logInfo $ "insert message: " <> display msg
  runIF (\HelloRepository {..} -> insertMessage msg)

selectImpl :: (Has LogFunc env, Has1 HelloRepository env) => RIO env [Text]
selectImpl = do
  logInfo "select message"
  runIF (\HelloRepository {..} -> selectMessage)

helloImpl :: (Has LogFunc env) => RIO env Hello
helloImpl = do
  logInfo "GET Hello"
  pure Hello

worldImpl :: (Has LogFunc env) => RIO env Hello
worldImpl = do
  logInfo "GET World"
  pure World

errorImpl :: (Has LogFunc env) => RIO env ()
errorImpl = do
  logInfo "GET Error"
  throwIO err400

fatalImpl :: (Has LogFunc env) => RIO env ()
fatalImpl = do
  logInfo "GET Fatal"
  undefined
