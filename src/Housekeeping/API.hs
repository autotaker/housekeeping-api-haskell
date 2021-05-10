{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Housekeeping.API
  ( API,
    app,
    api,
    Env,
  )
where

import Control.Env.Hierarchical
import Control.Monad.Except
import Data.Pool (Pool)
import Database.PostgreSQL.Simple (Connection)
import Housekeeping.DataSource (Database, HasConnectionPool (..), HasTransactionManager (..), TransactionManager, databaseImpl, defaultTransactionManager)
import qualified Housekeeping.Service.Hello as Hello
import RIO
  ( LogFunc,
    RIO,
    catch,
    runRIO,
  )
import Servant

type API = "hello" :> Hello.API

data Env = Env LogFunc (Pool Connection) (TransactionManager Connection) (Database Env)

deriveEnv ''Env

instance HasConnectionPool Env where
  type IConnection Env = Connection
  connectionPoolL = getL

instance HasTransactionManager Env where
  transactionManagerL = getL

api :: Proxy API
api = Proxy

app :: LogFunc -> Pool Connection -> Application
app lf pool = serve api $ hoistServer api nt Hello.server
  where
    nt :: RIO Env a -> Handler a
    nt action = do
      transactionManager <- liftIO defaultTransactionManager
      let env = Env lf pool transactionManager databaseImpl
      Handler $
        ExceptT $
          (Right <$> runRIO env action)
            `catch` (pure . Left)
