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
import Housekeeping.DataSource (Database, IConnection, TransactionManager, databaseImpl, defaultTransactionManager)
import qualified Housekeeping.Service.Auth as Auth
import qualified Housekeeping.Service.Hello as Hello
import Housekeeping.Session
import RIO
  ( LogFunc,
    RIO,
    catch,
    runRIO,
  )
import Servant

type API =
  "hello" :> Hello.API
    :<|> "auth" :> Auth.API

data Env
  = Env
      LogFunc
      (Pool Connection)
      (TransactionManager Connection)
      (Database Env)
      SessionConfig

deriveEnv ''Env

type instance IConnection Env = Connection

api :: Proxy API
api = Proxy

app :: LogFunc -> Pool Connection -> SessionConfig -> Application
app lf pool authConfig = serve api $ hoistServer api nt (Hello.server :<|> Auth.server)
  where
    nt :: RIO Env a -> Handler a
    nt action = do
      transactionManager <- liftIO defaultTransactionManager
      let env = Env lf pool transactionManager databaseImpl authConfig
      Handler $
        ExceptT $
          (Right <$> runRIO env action)
            `catch` (pure . Left)
