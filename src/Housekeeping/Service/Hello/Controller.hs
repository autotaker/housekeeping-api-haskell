{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}

module Housekeeping.Service.Hello.Controller
  ( API,
    api,
    server,
    HelloController (..),
  )
where

import Data.Aeson (FromJSON)
import Housekeeping.Service.Hello.Model (Hello)
import RIO
  ( Generic,
    Text,
  )
import Servant
import Web.FormUrlEncoded (FromForm)

newtype MessageForm = MessageForm {message :: Text}
  deriving (Eq, Ord, Show, Generic)

instance FromJSON MessageForm

instance FromForm MessageForm

type API =
  "hello" :> Get '[JSON] Hello
    :<|> "world" :> Get '[JSON] Hello
    :<|> "error" :> Get '[JSON] ()
    :<|> "fatal" :> Get '[JSON] ()
    :<|> "message" :> Get '[JSON] [Text]
    :<|> "message" :> ReqBody '[JSON, FormUrlEncoded] MessageForm
      :> Post '[JSON] ()

class HelloController m where
  helloHandler :: m Hello
  worldHandler :: m Hello
  errorHandler :: m ()
  fatalHandler :: m ()
  selectHandler :: m [Text]
  insertHandler :: Text -> m ()

{-# INLINE server #-}
server :: HelloController m => ServerT API m
server =
  helloHandler
    :<|> worldHandler
    :<|> errorHandler
    :<|> fatalHandler
    :<|> selectHandler
    :<|> insertHandler . message

api :: Proxy API
api = Proxy