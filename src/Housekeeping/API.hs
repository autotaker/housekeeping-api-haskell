{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Housekeeping.API
  ( API,
    server,
    app,
    api,
  )
where

import Control.Monad.Except
import qualified Housekeeping.Service.Hello.Controller as Hello
import qualified Housekeeping.Service.Hello.Handler as Hello
import RIO
  ( Generic,
    RIO,
    SimpleApp,
    catch,
    logInfo,
    runRIO,
    throwIO,
  )
import Servant

type API = "hello" :> Hello.API

server :: ServerT API (RIO SimpleApp)
server = hoistServer api helloNT Hello.server
  where
    helloNT = Hello.helloController

api :: Proxy API
api = Proxy

app :: SimpleApp -> Application
app env = serve api $ hoistServer api nt server
  where
    nt action =
      Handler $
        ExceptT $
          (Right <$> runRIO env action)
            `catch` (pure . Left)
