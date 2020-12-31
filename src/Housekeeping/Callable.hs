{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}

module Housekeeping.Callable (call) where

import RIO

class Callable env method where
  joinArgs :: RIO env method -> method

instance Callable env (RIO env a) where
  joinArgs = join

instance Callable env a => Callable env (b -> a) where
  joinArgs getter arg = joinArgs $ getter <*> pure arg

call :: Callable env method => SimpleGetter env method -> method
call method = joinArgs (view method)