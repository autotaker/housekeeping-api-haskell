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
import Housekeeping.DataSource (HasDataSource (..))
import qualified Housekeeping.Service.Hello as Hello
import RIO
  ( HasLogFunc (..),
    LogFunc,
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
