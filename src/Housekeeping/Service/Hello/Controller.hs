{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}

module Housekeeping.Service.Hello.Controller
  ( API,
    api,
    server,
    MessageForm (..),
  )
where

import Control.Env.Hierarchical
import Data.Aeson (FromJSON, ToJSON)
import Housekeeping.Service.Hello.Interface
  ( HelloHandler,
    errorHandler,
    fatalHandler,
    helloHandler,
    insertHandler,
    selectHandler,
    worldHandler,
  )
import Housekeeping.Service.Hello.Model (Hello)
import RIO
  ( Generic,
    RIO,
    Text,
  )
import Servant
  ( FormUrlEncoded,
    Get,
    HasServer (ServerT),
    JSON,
    Post,
    Proxy (..),
    ReqBody,
    type (:<|>) (..),
    type (:>),
  )
import Web.FormUrlEncoded (FromForm, ToForm)

newtype MessageForm = MessageForm {message :: Text}
  deriving (Eq, Ord, Show, Generic)

instance FromJSON MessageForm

instance FromForm MessageForm

instance ToJSON MessageForm

instance ToForm MessageForm

type API =
  "hello" :> Get '[JSON] Hello
    :<|> "world" :> Get '[JSON] Hello
    :<|> "error" :> Get '[JSON] ()
    :<|> "fatal" :> Get '[JSON] ()
    :<|> "message" :> Get '[JSON] [Text]
    :<|> "message" :> ReqBody '[JSON, FormUrlEncoded] MessageForm
      :> Post '[JSON] ()

server :: Has1 HelloHandler env => ServerT API (RIO env)
server =
  runIF helloHandler
    :<|> runIF worldHandler
    :<|> runIF errorHandler
    :<|> runIF fatalHandler
    :<|> runIF selectHandler
    :<|> (\msg -> runIF (\h -> insertHandler h $ message msg))

api :: Proxy API
api = Proxy