{-# LANGUAGE TemplateHaskell #-}

module Housekeeping.Service.Auth.Interface where

import Housekeeping.Service.Auth.Model
  ( HashedPassword,
    PasswordAuth,
    PlainPassword,
    User,
    UserName,
  )
import Lens.Micro.Platform (Lens', SimpleGetter, makeLenses)
import RIO (RIO)
import Servant.Auth.Server
  ( AuthResult,
    CookieSettings,
    JWTSettings,
  )

data AuthHandler env = AuthHandler
  { _signinHandler :: UserName -> PlainPassword -> RIO env (AuthResult User),
    _signupHandler :: UserName -> PlainPassword -> RIO env (Maybe User)
  }

makeLenses ''AuthHandler

class HasAuthHandler env where
  authHandlerL :: Lens' env (AuthHandler env)

data AuthConfig = AuthConfig
  { _cookieSettings :: CookieSettings,
    _jwtSettings :: JWTSettings
  }

makeLenses ''AuthConfig

data UserRepository env = UserRepository
  { _findUserByUserName :: UserName -> RIO env (Maybe User),
    _createUser :: User -> RIO env User
  }

makeLenses ''UserRepository

class ViewUserRepository env where
  userRepositoryV :: SimpleGetter env (UserRepository env)

data AuthRepository env = AuthRepository
  { _findPasswordAuthByUserName :: UserName -> RIO env (Maybe PasswordAuth),
    _upsertPasswordAuth :: PasswordAuth -> RIO env ()
  }

makeLenses ''AuthRepository

class ViewAuthRepository env where
  authRepositoryV :: SimpleGetter env (AuthRepository env)

class HasAuthConfig env where
  authConfigL :: Lens' env AuthConfig

newtype PasswordHasher env = PasswordHasher
  { _hashPassword :: PlainPassword -> RIO env HashedPassword
  }

makeLenses ''PasswordHasher

class ViewPasswordHasher env where
  passwordHasherV :: SimpleGetter env (PasswordHasher env)
