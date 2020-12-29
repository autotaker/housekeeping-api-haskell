{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}

module Housekeeping.Service.Hello.Handler
  ( HelloControllerImpl (..),
    runHelloController,
    runHelloControllerSimpleApp,
  )
where

import Housekeeping.Service.Hello.Controller (HelloController (..))
import Housekeeping.Service.Hello.Model (Hello (..))
import Housekeeping.Service.Hello.Repository
import RIO
import Servant.Server

newtype HelloControllerImpl env a = HelloControllerImpl
  {helloController :: RIO env a}

runHelloController :: env -> HelloControllerImpl env a -> IO a
runHelloController env (HelloControllerImpl action) =
  runRIO env action

runHelloControllerSimpleApp :: HelloControllerImpl SimpleApp a -> IO a
runHelloControllerSimpleApp (HelloControllerImpl action) =
  runSimpleApp action

deriving instance Monad (HelloControllerImpl env)

deriving instance MonadIO (HelloControllerImpl env)

deriving instance MonadReader env (HelloControllerImpl env)

deriving instance Applicative (HelloControllerImpl env)

deriving instance Functor (HelloControllerImpl env)

instance (HasLogFunc env, HasDataSource env) => HelloController (HelloControllerImpl env) where
  helloHandler = helloHandlerImpl
  worldHandler = worldHandlerImpl
  errorHandler = errorHandlerImpl
  fatalHandler = fatalHandlerImpl
  selectHandler = selectHandlerImpl
  insertHandler = insertHandlerImpl

insertHandlerImpl :: (HasCallStack, HasLogFunc env, HasDataSource env) => Text -> HelloControllerImpl env ()
insertHandlerImpl msg = do
  logInfo $ "insert message: " <> display msg
  HelloControllerImpl $ insertMessage msg

selectHandlerImpl :: (HasCallStack, HasLogFunc env, HasDataSource env) => HelloControllerImpl env [Text]
selectHandlerImpl = do
  logInfo "select message"
  HelloControllerImpl selectMessage

helloHandlerImpl :: (HasCallStack, HasLogFunc env) => HelloControllerImpl env Hello
helloHandlerImpl = do
  logInfo "GET Hello"
  pure Hello

worldHandlerImpl :: (HasCallStack, HasLogFunc env) => HelloControllerImpl env Hello
worldHandlerImpl = do
  logInfo "GET World"
  pure World

errorHandlerImpl :: (HasCallStack, HasLogFunc env) => HelloControllerImpl env ()
errorHandlerImpl = do
  logInfo "GET Error"
  throwIO err400

fatalHandlerImpl :: (HasCallStack, HasLogFunc env) => HelloControllerImpl env ()
fatalHandlerImpl = do
  logInfo "GET Fatal"
  undefined
