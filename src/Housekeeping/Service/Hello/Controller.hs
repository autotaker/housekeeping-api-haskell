{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators #-}

module Housekeeping.Service.Hello.Controller
  ( API,
    api,
    server,
    HelloController (..),
    HasHelloController (..),
    MessageForm (..),
  )
where

import Data.Aeson (FromJSON, ToJSON)
import Housekeeping.Service.Hello.Model (Hello)
import RIO
  ( Generic,
    RIO,
    SimpleGetter,
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

data HelloController env = HelloController
  { helloHandler :: RIO env Hello,
    worldHandler :: RIO env Hello,
    errorHandler :: RIO env (),
    fatalHandler :: RIO env (),
    selectHandler :: RIO env [Text],
    insertHandler :: Text -> RIO env ()
  }

class HasHelloController env where
  helloControllerL :: SimpleGetter env (HelloController env)

server :: HelloController env -> ServerT API (RIO env)
server HelloController {..} =
  helloHandler
    :<|> worldHandler
    :<|> errorHandler
    :<|> fatalHandler
    :<|> selectHandler
    :<|> insertHandler . message

api :: Proxy API
api = Proxy