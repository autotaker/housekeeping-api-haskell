{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Housekeeping.API
  ( API,
    server,
    app,
    api,
    Env (..),
  )
where

import Control.Monad.Except
import Data.Pool (Pool)
import Database.PostgreSQL.Simple (Connection)
import qualified Housekeeping.Service.Hello.Controller as Hello
import qualified Housekeeping.Service.Hello.Handler as Hello
import Housekeeping.Service.Hello.Repository (HasDataSource (..))
import RIO
  ( HasLogFunc (..),
    LogFunc,
    RIO,
    catch,
    lens,
    runRIO,
  )
import Servant

type API = "hello" :> Hello.API

data Env = Env
  { logFunc :: LogFunc,
    dataSource :: Pool Connection
  }

instance HasLogFunc Env where
  logFuncL = lens logFunc (\x y -> x {logFunc = y})

instance HasDataSource Env where
  dataSourceL = lens dataSource (\x y -> x {dataSource = y})

server :: ServerT API (RIO Env)
server = hoistServer api helloNT Hello.server
  where
    helloNT = Hello.helloController

api :: Proxy API
api = Proxy

app :: Env -> Application
app env = serve api $ hoistServer api nt server
  where
    nt action =
      Handler $
        ExceptT $
          (Right <$> runRIO env action)
            `catch` (pure . Left)
