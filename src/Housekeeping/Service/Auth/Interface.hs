{-# LANGUAGE TemplateHaskell #-}

module Housekeeping.Service.Auth.Interface where

import Housekeeping.Service.Auth.Model
  ( HashedPassword,
    PasswordAuth,
    PlainPassword,
    User,
    UserId,
  )
import Lens.Micro.Platform (Lens', makeLenses)
import RIO (RIO)
import Servant.Auth.Server
  ( AuthResult,
    CookieSettings,
    JWTSettings,
  )

data AuthHandler env = AuthHandler
  { _signinHandler :: UserId -> PlainPassword -> RIO env (AuthResult User),
    _signupHandler :: UserId -> PlainPassword -> RIO env (Maybe User)
  }

class HasAuthHandler env where
  authHandlerL :: Lens' env (AuthHandler env)

data AuthConfig = AuthConfig
  { _cookieSettings :: CookieSettings,
    _jwtSettings :: JWTSettings
  }

data UserRepository env = UserRepository
  { _findUserByUserId :: UserId -> RIO env (Maybe User),
    _createUser :: User -> RIO env User
  }

data AuthRepository env = AuthRepository
  { _findPasswordAuthByUserId :: UserId -> RIO env (Maybe PasswordAuth),
    _upsertPasswordAuth :: PasswordAuth -> RIO env ()
  }

class HasAuthConfig env where
  authConfigL :: Lens' env AuthConfig

makeLenses ''AuthConfig

makeLenses ''AuthHandler
