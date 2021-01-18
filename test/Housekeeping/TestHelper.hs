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

callDyn :: forall a b. Typeable a => Matcher a -> Matcher (Event Dynamic b)
callDyn matcher =
  call $ \dVal -> Just True == (matcher <$> fromDynamic dVal)