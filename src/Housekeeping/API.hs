{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
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
import Housekeeping.DataSource (HasConnectionPool (..), HasTransactionManager (..), TransactionManager)
import qualified Housekeeping.Service.Hello as Hello
import Lens.Micro.Platform
import RIO
  ( HasLogFunc (..),
    LogFunc,
    catch,
    runRIO,
  )
import Servant

type API = "hello" :> Hello.API

data Env = Env
  { _logFunc :: LogFunc,
    _connectionPool :: Pool Connection,
    _transactionManager :: TransactionManager
  }

makeLenses ''Env

instance HasLogFunc Env where
  logFuncL = logFunc

instance HasConnectionPool Env where
  connectionPoolL = connectionPool

instance HasTransactionManager Env where
  transactionManagerL = transactionManager

api :: Proxy API
api = Proxy

app :: Env -> Application
app env = serve api $ hoistServer api nt Hello.server
  where
    nt action =
      Handler $
        ExceptT $
          (Right <$> runRIO env action)
            `catch` (pure . Left)
