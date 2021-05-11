{-# LANGUAGE TemplateHaskell #-}

module Housekeeping.Session where

import Lens.Micro.Platform (makeLenses)
import Servant.Auth.Server (CookieSettings, JWTSettings)

data SessionConfig = SessionConfig
  { _cookieSettings :: CookieSettings,
    _jwtSettings :: JWTSettings
  }

makeLenses ''SessionConfig