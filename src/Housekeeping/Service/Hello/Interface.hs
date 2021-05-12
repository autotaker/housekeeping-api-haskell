{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}

module Housekeeping.Service.Hello.Interface where

import Housekeeping.Service.Hello.Model
import Housekeeping.Session (User)
import RIO

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
    insertHandler :: Text -> RIO env (),
    secretHandler :: User -> RIO env Hello
  }
