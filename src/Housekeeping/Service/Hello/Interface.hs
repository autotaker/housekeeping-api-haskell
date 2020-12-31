{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}

module Housekeeping.Service.Hello.Interface where

import Housekeeping.Service.Hello.Model
import RIO

class HasHelloRepository env where
  helloRepositoryL :: SimpleGetter env (HelloRepository env)

data HelloRepository env = HelloRepository
  { insertMessage :: Text -> RIO env (),
    selectMessage :: RIO env [Text]
  }

data HelloHandler env = HelloHandler
  { helloHandler :: RIO env Hello,
    worldHandler :: RIO env Hello,
    errorHandler :: RIO env (),
    fatalHandler :: RIO env (),
    selectHandler :: RIO env [Text],
    insertHandler :: Text -> RIO env ()
  }

class HasHelloHandler env where
  helloHandlerL :: SimpleGetter env (HelloHandler env)