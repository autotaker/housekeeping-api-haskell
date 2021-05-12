{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Housekeeping.Service.Auth (api, server, API) where

import Control.Env.Hierarchical
import Housekeeping.DataSource
import Housekeeping.Prelude (LogFunc, MonadReader (ask), RIO, runRIO)
import Housekeeping.Service.Auth.Controller (API, api)
import qualified Housekeeping.Service.Auth.Controller as Controller
import Housekeeping.Service.Auth.Handler (authHandlerImpl, passwordHasherImpl)
import Housekeeping.Service.Auth.Interface
import Housekeeping.Service.Auth.Repository (authRepositoryImpl, userRepositoryImpl)
import Housekeeping.Session
import Servant.Server

data AuthEnv env
  = AuthEnv
      (AuthHandler (AuthEnv env))
      (AuthRepository (AuthEnv env))
      (UserRepository (AuthEnv env))
      (PasswordHasher (AuthEnv env))
      (Extends env)

deriveEnv ''AuthEnv

type instance IConnection (AuthEnv env) = IConnection env

server :: forall env. (Has LogFunc env, Has1 Database env, Has SessionConfig env, HasTransactionManager env) => ServerT Controller.API (RIO env)
server = hoistServer Controller.api nt Controller.server
  where
    nt :: RIO (AuthEnv env) a -> RIO env a
    nt action = do
      env <- ask
      let env' =
            AuthEnv
              authHandlerImpl
              authRepositoryImpl
              userRepositoryImpl
              passwordHasherImpl
              (Extends env)
      runRIO env' action
