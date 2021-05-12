{-# LANGUAGE DeriveGeneric #-}

module Housekeeping.Service.Hello.Model (Hello (..)) where

import Data.Aeson
import GHC.Generics
import Housekeeping.Prelude (Text)

data Hello = Hello | World | Secret Text
  deriving (Eq, Show, Generic)

instance ToJSON Hello

instance FromJSON Hello
