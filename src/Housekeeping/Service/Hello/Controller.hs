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

import Control.Method (invoke)
import Data.Aeson (FromJSON, ToJSON)
import Housekeeping.Service.Hello.Interface
  ( HasHelloHandler (helloHandlerL),
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

server :: HasHelloHandler env => ServerT API (RIO env)
server =
  invoke (helloHandlerL . helloHandler)
    :<|> invoke (helloHandlerL . worldHandler)
    :<|> invoke (helloHandlerL . errorHandler)
    :<|> invoke (helloHandlerL . fatalHandler)
    :<|> invoke (helloHandlerL . selectHandler)
    :<|> invoke (helloHandlerL . insertHandler) . message

api :: Proxy API
api = Proxy