{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
module Housekeeping.API(
    API,
    Hello,
    server,
    app,
    api) where

import           Data.Aeson
import           GHC.Generics
import           Servant

type API = "hello" :> Get '[JSON] Hello
         :<|> "world" :> Get '[JSON] Hello

data Hello = Hello | World deriving(Eq, Show, Generic)

instance ToJSON Hello

server :: Server API
server = helloHandler :<|> worldHandler
    where
    helloHandler = pure Hello
    worldHandler = pure World

api :: Proxy API
api = Proxy

app :: Application
app = serve api server

