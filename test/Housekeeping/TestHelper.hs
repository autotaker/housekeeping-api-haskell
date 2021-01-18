{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Housekeeping.TestHelper where

import Data.Dynamic
import Data.Typeable
import Test.Method

dyn :: forall a b. (Typeable a, Typeable b) => a -> b
dyn a =
  case cast a of
    Just b -> b
    Nothing ->
      error $
        "cannot cast "
          ++ show (typeOf a)
          ++ " to "
          ++ show (typeRep (Proxy :: Proxy b))

dynEq :: (Typeable a, Typeable b, Eq b) => b -> Matcher a
dynEq b a =
  case cast a of
    Just b' -> b == b'
    Nothing -> False

callDyn :: forall a b. Typeable a => Matcher a -> Matcher (Event Dynamic b)
callDyn matcher =
  call $ \dVal -> Just True == (matcher <$> fromDynamic dVal)