{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}

module Housekeeping.Service.Hello.Interface where

import Housekeeping.Service.Hello.Model
import Lens.Micro.Platform
import RIO

class HasHelloRepository env where
  helloRepositoryL :: Lens' env (HelloRepository env)

class ViewHelloRepository env where
  helloRepositoryV :: SimpleGetter env (HelloRepository env)
  default helloRepositoryV :: HasHelloRepository env => SimpleGetter env (HelloRepository env)
  helloRepositoryV = helloRepositoryL

data HelloRepository env = HelloRepository
  { _insertMessage :: Text -> RIO env (),
    _selectMessage :: RIO env [Text]
  }

makeLenses ''HelloRepository

data HelloHandler env = HelloHandler
  { _helloHandler :: RIO env Hello,
    _worldHandler :: RIO env Hello,
    _errorHandler :: RIO env (),
    _fatalHandler :: RIO env (),
    _selectHandler :: RIO env [Text],
    _insertHandler :: Text -> RIO env ()
  }

makeLenses ''HelloHandler

class HasHelloHandler env where
  helloHandlerL :: Lens' env (HelloHandler env)

class ViewHelloHandler env where
  helloHandlerV :: SimpleGetter env (HelloHandler env)
  default helloHandlerV :: HasHelloHandler env => SimpleGetter env (HelloHandler env)
  helloHandlerV = helloHandlerL
