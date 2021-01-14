{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

module Housekeeping.Service.Auth.Model where

import Data.Aeson (FromJSON, ToJSON)
import Lens.Micro.Platform (makeLenses)
import RIO (ByteString, Generic, Text)
import Servant.Auth.Server (FromJWT, ToJWT)

newtype User = User {_userId :: UserId}
  deriving (Show, Eq, Ord, Generic)

instance FromJSON User

instance ToJSON User

instance FromJWT User

instance ToJWT User

type UserId = Text

newtype PlainPassword = PlainPassword ByteString
  deriving (Eq, Ord, Show)

newtype HashedPassword = HashedPassword ByteString
  deriving (Eq, Ord, Show)

makeLenses ''User