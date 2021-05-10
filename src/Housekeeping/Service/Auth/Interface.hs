{-# LANGUAGE TemplateHaskell #-}

module Housekeeping.Service.Auth.Interface where

import Housekeeping.Service.Auth.Model
  ( HashedPassword,
    PasswordAuth,
    PlainPassword,
    User,
    UserName,
  )
import Lens.Micro.Platform (makeLenses)
import RIO (RIO)
import Servant.Auth.Server
  ( AuthResult,
    CookieSettings,
    JWTSettings,
  )

data AuthHandler env = AuthHandler
  { signinHandler :: UserName -> PlainPassword -> RIO env (AuthResult User),
    signupHandler :: UserName -> PlainPassword -> RIO env (Maybe User)
  }

data AuthConfig = AuthConfig
  { _cookieSettings :: CookieSettings,
    _jwtSettings :: JWTSettings
  }

makeLenses ''AuthConfig

data UserRepository env = UserRepository
  { findUserByUserName :: UserName -> RIO env (Maybe User),
    createUser :: User -> RIO env User
  }

data AuthRepository env = AuthRepository
  { findPasswordAuthByUserName :: UserName -> RIO env (Maybe PasswordAuth),
    upsertPasswordAuth :: PasswordAuth -> RIO env ()
  }

newtype PasswordHasher env = PasswordHasher
  { hashPassword :: PlainPassword -> RIO env HashedPassword
  }
