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
import Housekeeping.Prelude (view)
import Housekeeping.Service.Hello.Interface
  (helloHandler,  HelloHandler,
    errorHandler,
    fatalHandler,
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
  runIF (view helloHandler)
    :<|> runIF (view worldHandler)
    :<|> runIF (view errorHandler)
    :<|> runIF (view fatalHandler)
    :<|> runIF (view selectHandler)
    :<|> (\msg -> runIF (\h -> view insertHandler h $ message msg))

api :: Proxy API
api = Proxy