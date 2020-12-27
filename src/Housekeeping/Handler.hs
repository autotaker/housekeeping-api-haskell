{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
module Housekeeping.Handler(
    helloHandler,
    worldHandler,
    errorHandler,
    fatalHandler) where

import           Housekeeping.Model (Hello (..))
import           RIO

import           Servant.Server

helloHandler :: (HasCallStack, HasLogFunc env) => RIO env Hello
helloHandler = do
    logInfo "GET Hello"
    pure Hello

worldHandler :: (HasCallStack, HasLogFunc env) => RIO env Hello
worldHandler = do
    logInfo "GET World"
    pure World

errorHandler :: (HasCallStack, HasLogFunc env) => RIO env ()
errorHandler = do
    logInfo "GET Error"
    throwIO err400

fatalHandler :: (HasCallStack, HasLogFunc env) => RIO env ()
fatalHandler = do
    logInfo "GET Fatal"
    undefined
