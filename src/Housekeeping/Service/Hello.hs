{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}

module Housekeeping.Service.Hello (api, server, API) where

import Control.Monad.Reader (withReaderT)
import Housekeeping.Cascade
import Housekeeping.DataSource (HasConnectionPool (..), HasTransactionManager (..), ViewDatabase (..))
import Housekeeping.Service.Hello.Controller (API, api)
import qualified Housekeeping.Service.Hello.Controller as Controller
import Housekeeping.Service.Hello.Handler (helloHandlerImpl)
import Housekeeping.Service.Hello.Interface (ViewHelloHandler (..), ViewHelloRepository (..))
import Housekeeping.Service.Hello.Repository (helloRepositoryImpl)
import RIO (HasLogFunc (..), Lens', RIO (..), lens, to, view)
import Servant.Server (HasServer (ServerT), hoistServer)

newtype HelloEnv env = HelloEnv {inherit :: env}

instance HasSuper (HelloEnv env) where
  type Super (HelloEnv env) = env
  superL = inheritL

instance ViewDatabase env => ViewDatabase (HelloEnv env) where
  databaseV = superL . databaseV . to (contramap (view superL))

instance (HasLogFunc env, ViewDatabase env) => ViewHelloHandler (HelloEnv env) where
  helloHandlerV = to (const helloHandlerImpl)

instance (HasLogFunc env, ViewDatabase env) => ViewHelloRepository (HelloEnv env) where
  helloRepositoryV = to (const helloRepositoryImpl)

instance HasLogFunc env => HasLogFunc (HelloEnv env) where
  logFuncL = inheritL . logFuncL

inheritL :: Lens' (HelloEnv env) env
inheritL = lens inherit (\x y -> x {inherit = y})

instance HasConnectionPool env => HasConnectionPool (HelloEnv env) where
  type IConnection (HelloEnv env) = IConnection env
  connectionPoolL = inheritL . connectionPoolL

instance HasTransactionManager env => HasTransactionManager (HelloEnv env) where
  transactionManagerL = inheritL . transactionManagerL

server :: (HasLogFunc env, ViewDatabase env) => ServerT API (RIO env)
server = hoistServer api nt Controller.server
  where
    nt action =
      RIO $ withReaderT HelloEnv $ unRIO action
