{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module Housekeeping.Session where

import Data.Aeson
import Database.PostgreSQL.Simple (FromRow)
import Housekeeping.Prelude (Generic, Text)
import Lens.Micro.Platform (makeLenses)
import Servant.Auth.Server (CookieSettings, FromJWT, JWTSettings, ToJWT, defaultCookieSettings, defaultJWTSettings, generateKey)

data SessionConfig = SessionConfig
  { _cookieSettings :: CookieSettings,
    _jwtSettings :: JWTSettings
  }

makeLenses ''SessionConfig

type UserId = Int

type UserName = Text

data User = User {_userName :: UserName, _userId :: UserId}
  deriving (Show, Eq, Ord, Generic)

makeLenses ''User

instance FromJSON User

instance ToJSON User

instance FromJWT User

instance ToJWT User

instance FromRow User

defaultSessionConfig :: IO SessionConfig
defaultSessionConfig = do
  jwk <- generateKey
  pure
    SessionConfig
      { _jwtSettings = defaultJWTSettings jwk,
        _cookieSettings = defaultCookieSettings
      }