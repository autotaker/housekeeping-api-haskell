{-# LANGUAGE DeriveGeneric #-}

module Housekeeping.Service.Hello.Model (Hello (..)) where

import Data.Aeson
import GHC.Generics

data Hello = Hello | World deriving (Eq, Show, Generic)

instance ToJSON Hello

instance FromJSON Hello
