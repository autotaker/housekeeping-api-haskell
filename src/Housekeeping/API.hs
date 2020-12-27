{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Housekeeping.API
  ( API,
    Hello,
    server,
    app,
    api,
  )
where

import Control.Monad.Except
import Housekeeping.Handler
import Housekeeping.Model
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

type API =
  "hello" :> Get '[JSON] Hello
    :<|> "world" :> Get '[JSON] Hello
    :<|> "error" :> Get '[JSON] ()
    :<|> "fatal" :> Get '[JSON] ()

server :: ServerT API (RIO SimpleApp)
server =
  helloHandler
    :<|> worldHandler
    :<|> errorHandler
    :<|> fatalHandler

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
