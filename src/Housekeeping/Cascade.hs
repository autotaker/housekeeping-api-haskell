{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Housekeeping.Cascade
  ( HasSuper (Super, superL),
    mapEnvMethod,
    module Data.Functor.Contravariant,
  )
where

import Control.Method (Method (..), invoke)
import Data.Functor.Contravariant
import Lens.Micro.Platform
import RIO

class HasSuper x where
  type Super x
  superL :: Lens' x (Super x)

{-# INLINE mapEnvMethod #-}
mapEnvMethod ::
  ( Method method,
    Method method',
    Base method ~ RIO env,
    Base method' ~ RIO env',
    Ret method ~ Ret method',
    Args method ~ Args method'
  ) =>
  (env' -> env) ->
  method ->
  method'
mapEnvMethod f method = curryMethod $ \args -> do
  env <- view $ to f
  runRIO env (uncurryMethod method args)
