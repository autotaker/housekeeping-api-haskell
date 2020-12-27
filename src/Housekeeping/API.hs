{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}
module Housekeeping.API(
    API,
    Hello,
    server,
    app,
    api) where

import           RIO                  (Generic, Proxy (..), RIO, SimpleApp,
                                       catch, logInfo, runRIO, throwIO)

import           Control.Monad.Except
import           Data.Aeson
import           GHC.Generics
import           Servant

type API = "hello" :> Get '[JSON] Hello
         :<|> "world" :> Get '[JSON] Hello
         :<|> "error" :> Get '[JSON] ()
         :<|> "fatal" :> Get '[JSON] ()

data Hello = Hello | World deriving(Eq, Show, Generic)

instance ToJSON Hello


server :: ServerT API (RIO SimpleApp)
server = helloHandler
    :<|> worldHandler
    :<|> errorHandler
    :<|> fatalHandler
    where
    helloHandler = do
        logInfo "GET Hello"
        pure Hello
    worldHandler = do
        logInfo "GET World"
        pure World
    errorHandler = do
        logInfo "GET Error"
        throwIO err400
    fatalHandler = do
        logInfo "GET Fatal"
        undefined

api :: Proxy API
api = Proxy

app :: SimpleApp -> Application
app env = serve api $ hoistServer api nt server
    where
    nt action = Handler $ ExceptT $
        (Right <$> runRIO env action)
            `catch` (pure . Left)


