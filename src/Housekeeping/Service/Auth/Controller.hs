{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}

module Housekeeping.Service.Auth.Controller where

import Control.Method (invoke)
import Data.Aeson (FromJSON, ToJSON)
import Data.Char (isAscii)
import Housekeeping.Service.Auth.Interface
  ( HasAuthConfig (..),
    HasAuthHandler (..),
    cookieSettings,
    jwtSettings,
    signinHandler,
    signupHandler,
  )
import Housekeeping.Service.Auth.Model (PlainPassword (..), User)
import Lens.Micro.Platform (makeLenses, view, (^.))
import RIO
  ( Generic,
    MonadIO (liftIO),
    MonadThrow (throwM),
    RIO,
    Text,
    encodeUtf8,
    unless,
  )
import qualified RIO.Text as T
import Servant
  ( HasServer (ServerT),
    Header,
    Headers,
    JSON,
    Post,
    Proxy (..),
    ReqBody,
    ServerError (errBody),
    err400,
    err401,
    err409,
    err500,
    type (:<|>) ((:<|>)),
    type (:>),
  )
import Servant.Auth.Server
  ( AuthResult (Authenticated),
    SetCookie,
    acceptLogin,
    clearSession,
  )

data PasswordForm = PasswordForm
  { _userId :: Text,
    _password :: Text
  }
  deriving (Eq, Ord, Show, Generic)

makeLenses ''PasswordForm

instance FromJSON PasswordForm

instance ToJSON PasswordForm

type API =
  "signin" :> ReqBody '[JSON] PasswordForm
    :> Post '[JSON] (AuthResponse User)
    :<|> "signup"
    :> ReqBody '[JSON] PasswordForm
    :> Post '[JSON] User
    :<|> "signout"
    :> Post '[JSON] (AuthResponse ())

type AuthResponse a =
  Headers '[Header "Set-Cookie" SetCookie, Header "Set-Cookie" SetCookie] a

api :: Proxy API
api = Proxy

server :: forall env. (HasAuthHandler env, HasAuthConfig env) => ServerT API (RIO env)
server = signin :<|> signup :<|> signout
  where
    signin :: PasswordForm -> RIO env (AuthResponse User)
    signin form = do
      let usernm = form ^. userId
          textPasswd = form ^. password
      unless (T.all isAscii textPasswd) $ throwM err400
      let passwd = PlainPassword $ encodeUtf8 textPasswd
      config <- view authConfigL
      res <- invoke (authHandlerL . signinHandler) usernm passwd
      case res of
        Authenticated user -> do
          mAccept <- liftIO $ acceptLogin (config ^. cookieSettings) (config ^. jwtSettings) user
          case mAccept of
            Just accept -> pure (accept user)
            Nothing -> throwM err500
        _ -> throwM err401

    signup :: PasswordForm -> RIO env User
    signup form = do
      let usernm = form ^. userId
          textPasswd = form ^. password
      unless (T.all isAscii textPasswd) $ throwM err400
      let passwd = PlainPassword $ encodeUtf8 textPasswd
      mUser <- invoke (authHandlerL . signupHandler) usernm passwd
      case mUser of
        Just user -> pure user
        Nothing -> throwM err409 {errBody = "The user ID is already taken"}

    signout :: RIO env (AuthResponse ())
    signout = do
      cookieCfg <- view $ authConfigL . cookieSettings
      pure $ clearSession cookieCfg ()
