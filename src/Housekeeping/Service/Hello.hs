{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}

module Housekeeping.Service.Hello (api, server, API) where

import Control.Monad.Reader (withReaderT)
import Housekeeping.DataSource (HasDataSource (..))
import Housekeeping.Service.Hello.Controller (API, api)
import qualified Housekeeping.Service.Hello.Controller as Controller
import Housekeeping.Service.Hello.Handler (helloHandlerImpl)
import Housekeeping.Service.Hello.Interface (HasHelloHandler (..), HasHelloRepository (..))
import Housekeeping.Service.Hello.Repository (helloRepositoryImpl)
import RIO (HasLogFunc (..), Lens', RIO (..), lens)
import Servant.Server (HasServer (ServerT), hoistServer)

newtype HelloEnv env = HelloEnv {inherit :: env}

instance (HasLogFunc env, HasDataSource env) => HasHelloHandler (HelloEnv env) where
  helloHandlerL = lens (const helloHandlerImpl) const

instance (HasLogFunc env, HasDataSource env) => HasHelloRepository (HelloEnv env) where
  helloRepositoryL = lens (const helloRepositoryImpl) const

instance HasLogFunc env => HasLogFunc (HelloEnv env) where
  logFuncL = inheritL . logFuncL

inheritL :: Lens' (HelloEnv env) env
inheritL = lens inherit (\x y -> x {inherit = y})

instance HasDataSource env => HasDataSource (HelloEnv env) where
  dataSourceL = inheritL . dataSourceL

server :: (HasLogFunc env, HasDataSource env) => ServerT API (RIO env)
server = hoistServer api nt Controller.server
  where
    nt action =
      RIO $ withReaderT HelloEnv $ unRIO action
