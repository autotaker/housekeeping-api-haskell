{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

module Housekeeping.Service.Auth.Model where

import Data.Aeson (FromJSON, ToJSON)
import Lens.Micro.Platform (makeLenses)
import RIO (ByteString, Generic, Text)
import Servant.Auth.Server (FromJWT, ToJWT)

type UserId = Text

newtype PlainPassword = PlainPassword ByteString
  deriving (Eq, Ord, Show)

newtype HashedPassword = HashedPassword ByteString
  deriving (Eq, Ord, Show)

data User = User {_userId :: UserId, _userSerialId :: Int}
  deriving (Show, Eq, Ord, Generic)

makeLenses ''User

instance FromJSON User

instance ToJSON User

instance FromJWT User

instance ToJWT User

data PasswordAuth = PasswordAuth
  { _passwordAuthUser :: User,
    _passwordAuthPass :: HashedPassword
  }
  deriving (Show, Eq, Ord, Generic)

makeLenses ''PasswordAuth
