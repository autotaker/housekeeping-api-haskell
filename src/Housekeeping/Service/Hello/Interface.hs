{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Housekeeping.Service.Hello.Interface where

import Control.Env.Hierarchical (Interface (IBase))
import Housekeeping.Service.Hello.Model
import Lens.Micro.Platform
import RIO

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
