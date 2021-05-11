{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Housekeeping.Service.Hello (api, server, API) where

import Control.Env.Hierarchical
import Control.Monad.Reader (withReaderT)
import Data.Proxy
import Housekeeping.DataSource (Database, IConnection)
import Housekeeping.Service.Hello.Controller (API, api)
import qualified Housekeeping.Service.Hello.Controller as Controller
import Housekeeping.Service.Hello.Handler (helloHandlerImpl)
import Housekeeping.Service.Hello.Interface
import Housekeeping.Service.Hello.Repository (helloRepositoryImpl)
import RIO (LogFunc, RIO (..))
import Servant.Auth.Server (CookieSettings, JWTSettings)
import Servant.Server (HasServer (ServerT, hoistServerWithContext))

data HelloEnv env
  = HelloEnv
      (HelloHandler (HelloEnv env))
      (HelloRepository (HelloEnv env))
      (Extends env)

deriveEnv ''HelloEnv

type instance IConnection (HelloEnv env) = IConnection env

server :: (Has LogFunc env, Has1 Database env) => ServerT API (RIO env)
server = hoistServerWithContext api ctxProxy nt Controller.server
  where
    ctxProxy :: Proxy '[JWTSettings, CookieSettings]
    ctxProxy = Proxy
    nt action =
      RIO $ withReaderT (HelloEnv helloHandlerImpl helloRepositoryImpl . Extends) $ unRIO action
