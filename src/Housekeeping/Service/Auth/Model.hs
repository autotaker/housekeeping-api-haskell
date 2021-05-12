{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

module Housekeeping.Service.Auth.Model where

import Database.PostgreSQL.Simple.FromField (FromField)
import Database.PostgreSQL.Simple.ToField (ToField)
import Housekeeping.Session (User)
import Lens.Micro.Platform (makeLenses)
import RIO (ByteString, Generic)

newtype PlainPassword = PlainPassword ByteString
  deriving (Eq, Ord, Show)

newtype HashedPassword = HashedPassword ByteString
  deriving (Eq, Ord, Show, FromField, ToField)

data PasswordAuth = PasswordAuth
  { _passwordAuthUser :: User,
    _passwordAuthPass :: HashedPassword
  }
  deriving (Show, Eq, Ord, Generic)

makeLenses ''PasswordAuth
