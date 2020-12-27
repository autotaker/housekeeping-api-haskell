{-# LANGUAGE DeriveGeneric #-}
module Housekeeping.Model(Hello(..)) where

import           Data.Aeson
import           GHC.Generics

data Hello = Hello | World deriving(Eq, Show, Generic)

instance ToJSON Hello
