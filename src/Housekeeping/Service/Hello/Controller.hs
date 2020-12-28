{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}

module Housekeeping.Service.Hello.Controller
  ( API,
    api,
    server,
    HelloController (..),
  )
where

import Housekeeping.Service.Hello.Model (Hello)
import RIO
  ( Generic,
    Lens',
    RIO,
    SimpleApp,
    catch,
    logInfo,
    runRIO,
    throwIO,
  )
import Servant

type API =
  "hello" :> Get '[JSON] Hello
    :<|> "world" :> Get '[JSON] Hello
    :<|> "error" :> Get '[JSON] ()
    :<|> "fatal" :> Get '[JSON] ()

class HelloController m where
  helloHandler :: m Hello
  worldHandler :: m Hello
  errorHandler :: m ()
  fatalHandler :: m ()

{-# INLINE server #-}
server :: HelloController m => ServerT API m
server =
  helloHandler
    :<|> worldHandler
    :<|> errorHandler
    :<|> fatalHandler

api :: Proxy API
api = Proxy