{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Housekeeping.API
  ( API,
    app,
    api,
    Env (..),
  )
where

import Control.Monad.Except
import Data.Pool (Pool)
import Database.PostgreSQL.Simple (Connection)
import Housekeeping.DataSource (HasConnectionPool (..), HasTransactionManager (..), TransactionManager, ViewDatabase (..), databaseImpl)
import qualified Housekeeping.Service.Hello as Hello
import Lens.Micro.Platform
import RIO
  ( HasLogFunc (..),
    LogFunc,
    RIO,
    catch,
    runRIO,
  )
import Servant

type API = "hello" :> Hello.API

data Env = Env
  { _logFunc :: LogFunc,
    _connectionPool :: Pool Connection,
    _transactionManager :: TransactionManager Connection
  }

makeLenses ''Env

instance HasLogFunc Env where
  logFuncL = logFunc

instance HasConnectionPool Env where
  type IConnection Env = Connection
  connectionPoolL = connectionPool

instance HasTransactionManager Env where
  transactionManagerL = transactionManager

instance ViewDatabase Env where
  databaseV = to (const databaseImpl)

api :: Proxy API
api = Proxy

app :: Env -> Application
app env = serve api $ hoistServer api nt Hello.server
  where
    nt :: RIO Env a -> Handler a
    nt action =
      Handler $
        ExceptT $
          (Right <$> runRIO env action)
            `catch` (pure . Left)
