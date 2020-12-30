{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}

module Housekeeping.Service.Hello (api, server, API) where

import Housekeeping.DataSource
import Housekeeping.Service.Hello.Controller (API, api)
import qualified Housekeeping.Service.Hello.Controller as Controller
import Housekeeping.Service.Hello.Handler (helloHandlerImpl)
import Housekeeping.Service.Hello.Interface (HasHelloHandler (..), HasHelloRepository (..), HelloHandler, HelloRepository)
import Housekeeping.Service.Hello.Repository (helloRepositoryImpl)
import RIO
import Servant.Server

data HelloEnv env = HelloEnv
  { helloHandler :: HelloHandler (HelloEnv env),
    helloRepository :: HelloRepository (HelloEnv env),
    inherit :: env
  }

instance HasHelloHandler (HelloEnv env) where
  helloHandlerL = lens helloHandler (\x y -> x {helloHandler = y})

instance HasHelloRepository (HelloEnv env) where
  helloRepositoryL = lens helloRepository (\x y -> x {helloRepository = y})

instance HasLogFunc env => HasLogFunc (HelloEnv env) where
  logFuncL = inheritL . logFuncL

inheritL :: Lens' (HelloEnv env) env
inheritL = lens inherit (\x y -> x {inherit = y})

instance HasDataSource env => HasDataSource (HelloEnv env) where
  dataSourceL = inheritL . dataSourceL

server :: (HasLogFunc env, HasDataSource env) => ServerT API (RIO env)
server = hoistServer api nt (Controller.server helloHandlerImpl)
  where
    helloEnv env =
      HelloEnv
        { helloHandler = helloHandlerImpl,
          helloRepository = helloRepositoryImpl,
          inherit = env
        }
    nt action = do
      env <- ask
      runRIO (helloEnv env) action
