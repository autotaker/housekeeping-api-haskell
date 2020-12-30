{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators #-}

module Housekeeping.Service.Hello.Controller
  ( API,
    api,
    server,
    MessageForm (..),
  )
where

import Data.Aeson (FromJSON, ToJSON)
import Housekeeping.Service.Hello.Interface
  ( HelloHandler (..),
  )
import Housekeeping.Service.Hello.Model (Hello)
import RIO
  ( Generic,
    RIO,
    Text,
  )
import Servant
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

server :: HelloHandler env -> ServerT API (RIO env)
server HelloHandler {..} =
  helloHandler
    :<|> worldHandler
    :<|> errorHandler
    :<|> fatalHandler
    :<|> selectHandler
    :<|> insertHandler . message

api :: Proxy API
api = Proxy