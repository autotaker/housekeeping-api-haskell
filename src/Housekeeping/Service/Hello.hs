{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Housekeeping.Service.Hello (api, server, API) where

import Control.Env.Hierarchical
import Control.Monad.Reader (withReaderT)
import Housekeeping.DataSource (Database, HasConnectionPool (..), HasTransactionManager (..))
import Housekeeping.Service.Hello.Controller (API, api)
import qualified Housekeeping.Service.Hello.Controller as Controller
import Housekeeping.Service.Hello.Handler (helloHandlerImpl)
import Housekeeping.Service.Hello.Interface
import Housekeeping.Service.Hello.Repository (helloRepositoryImpl)
import RIO (LogFunc, RIO (..))
import Servant.Server (HasServer (ServerT), hoistServer)

data HelloEnv env
  = HelloEnv
      (HelloHandler (HelloEnv env))
      (HelloRepository (HelloEnv env))
      (Extends env)

deriveEnv ''HelloEnv

instance HasConnectionPool env => HasConnectionPool (HelloEnv env) where
  type IConnection (HelloEnv env) = IConnection env
  connectionPoolL = superL . connectionPoolL

instance HasTransactionManager env => HasTransactionManager (HelloEnv env) where
  transactionManagerL = superL . transactionManagerL

server :: (Has LogFunc env, Has1 Database env) => ServerT API (RIO env)
server = hoistServer api nt Controller.server
  where
    nt action =
      RIO $ withReaderT (HelloEnv helloHandlerImpl helloRepositoryImpl . Extends) $ unRIO action
