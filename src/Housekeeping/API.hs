{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Housekeeping.API
  ( API,
    app,
    api,
    Env,
    WebRoot (WebRoot),
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
    (^.),
  )
import Servant
import Servant.Auth.Server

type API =
  "hello" :> Hello.API
    :<|> "auth" :> Auth.API
    :<|> Raw

newtype WebRoot = WebRoot FilePath deriving (Show, Eq)

data Env
  = Env
      LogFunc
      (Pool Connection)
      (TransactionManager Connection)
      (Database Env)
      SessionConfig
      WebRoot

deriveEnv ''Env

type instance IConnection Env = Connection

api :: Proxy API
api = Proxy

app :: LogFunc -> Pool Connection -> SessionConfig -> WebRoot -> Application
app lf pool config webRoot@(WebRoot rootPath) =
  serveWithContext api ctx $
    hoistServerWithContext api ctxProxy nt $
      Hello.server :<|> Auth.server :<|> fileServer
  where
    ctx :: Context '[JWTSettings, CookieSettings]
    ctx = (config ^. jwtSettings) :. (config ^. cookieSettings) :. EmptyContext
    ctxProxy :: Proxy '[JWTSettings, CookieSettings]
    ctxProxy = Proxy
    nt :: RIO Env a -> Handler a
    nt action = do
      transactionManager <- liftIO defaultTransactionManager
      let env =
            Env
              lf
              pool
              transactionManager
              databaseImpl
              config
              webRoot
      Handler $
        ExceptT $
          (Right <$> runRIO env action)
            `catch` (pure . Left)
    fileServer = serveDirectoryWebApp rootPath
