{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

module Housekeeping.Service.Auth.Model where

import Data.Aeson (FromJSON, ToJSON)
import Database.PostgreSQL.Simple (FromRow)
import Database.PostgreSQL.Simple.FromField (FromField)
import Lens.Micro.Platform (makeLenses)
import RIO (ByteString, Generic, Text)
import Servant.Auth.Server (FromJWT, ToJWT)
import Database.PostgreSQL.Simple.ToField (ToField)

type UserId = Int

type UserName = Text

newtype PlainPassword = PlainPassword ByteString
  deriving (Eq, Ord, Show)

newtype HashedPassword = HashedPassword ByteString
  deriving (Eq, Ord, Show, FromField, ToField)

data User = User {_userName :: UserName, _userId :: UserId}
  deriving (Show, Eq, Ord, Generic)

makeLenses ''User

instance FromJSON User

instance ToJSON User

instance FromJWT User

instance ToJWT User

instance FromRow User

data PasswordAuth = PasswordAuth
  { _passwordAuthUser :: User,
    _passwordAuthPass :: HashedPassword
  }
  deriving (Show, Eq, Ord, Generic)

makeLenses ''PasswordAuth
