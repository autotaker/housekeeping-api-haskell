{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TypeFamilies #-}

module Housekeeping.Service.Auth.HandlerSpec where

import Control.Method (invoke)
import Data.Pool
import Housekeeping.DataSource
import Housekeeping.Service.Auth.Handler
import Housekeeping.Service.Auth.Interface
import Housekeeping.Service.Auth.Model
import Lens.Micro.Platform
import RIO (ByteString, MonadReader (local), runRIO, void)
import Test.Hspec
import Test.Method

data Env = Env
  { _userRepository :: UserRepository Env,
    _authRepository :: AuthRepository Env,
    _passwordHasher :: PasswordHasher Env,
    _transactionManager :: TransactionManager MockConnection,
    _connectionPool :: Pool MockConnection
  }

data MockConnection = MockConnection

makeLenses ''Env

env :: Pool MockConnection -> Env
env pool =
  Env
    { _userRepository = userRepositoryMock,
      _authRepository = authRepositoryMock,
      _passwordHasher = passwordHasherMock,
      _transactionManager = defaultTransactionManager,
      _connectionPool = pool
    }

passwordHasherMock :: PasswordHasher Env
passwordHasherMock =
  PasswordHasher
    { _hashPassword = \ !_ ->
        pure $ HashedPassword "HASHED_PASSWORD"
    }

mockSalt :: ByteString
mockSalt = "$2y$04$akDsXE7raEDxa1btakPxWO"

authRepositoryMock :: AuthRepository Env
authRepositoryMock =
  AuthRepository
    { _findPasswordAuthByUserName = undefined,
      _upsertPasswordAuth = mockup $ do
        when anything `thenReturn` ()
    }

userRepositoryMock :: UserRepository Env
userRepositoryMock =
  UserRepository
    { _findUserByUserName = mockup $ do
        when (args (== "user1")) `thenReturn` Just user1
        when anything `thenReturn` Nothing,
      _createUser = mockup $ do
        when anything `thenReturn` user2
    }

user1, user2 :: User
user1 = User {_userName = "user1", _userId = 0}
user2 = User {_userName = "user2", _userId = 1}

instance ViewAuthHandler Env where
  authHandlerV = to $ const authHandlerImpl

instance ViewUserRepository Env where
  userRepositoryV = userRepository

instance ViewAuthRepository Env where
  authRepositoryV = authRepository

instance ViewPasswordHasher Env where
  passwordHasherV = passwordHasher

instance HasTransactionManager Env where
  transactionManagerL = transactionManager

instance Transactional MockConnection where
  begin _ = pure ()
  commit _ = pure ()
  rollback _ = pure ()

instance HasConnectionPool Env where
  type IConnection Env = MockConnection
  connectionPoolL = connectionPool

spec :: Spec
spec = do
  pool <- runIO $ createPool (pure MockConnection) (const $ pure ()) 1 0.5 1
  describe "signupHandler" $ do
    context "when the username is not registered yet" $ do
      let run usernm passwd =
            runRIO (env pool) $
              invoke (authHandlerV . signupHandler) usernm passwd
      it "returns `Just user`" $ do
        run "user2" (PlainPassword "password2") `shouldReturn` Just user2
      it "calls `createUser user`" $ do
        logs <- runRIO (env pool) $
          withMonitor_ $ \monitor ->
            local (userRepository . createUser %~ watch monitor) $
              void $ invoke (authHandlerV . signupHandler) "user2" (PlainPassword "password2")
        logs `shouldSatisfy` (== 1)
          `times` call (args (== User {_userName = "user2", _userId = -1}))
      it "calls `upsertPasswordAuth`" $ do
        logs <- runRIO (env pool) $
          withMonitor_ $ \monitor ->
            local (authRepository . upsertPasswordAuth %~ watch monitor) $
              void $ invoke (authHandlerV . signupHandler) "user2" (PlainPassword "password2")
        logs `shouldSatisfy` (== 1)
          `times` call (args (== PasswordAuth {_passwordAuthUser = user2, _passwordAuthPass = HashedPassword "HASHED_PASSWORD"}))
    context "when the username is already registered" $ do
      let run usernm passwd =
            runRIO (env pool) $
              invoke (authHandlerV . signupHandler) usernm passwd
      it "returns `Nothing`" $ do
        run "user1" (PlainPassword "password1") `shouldReturn` Nothing
      it "does not calls `createUser user`" $ do
        logs <- runRIO (env pool) $
          withMonitor_ $ \monitor ->
            local (userRepository . createUser %~ watch monitor) $
              void $ invoke (authHandlerV . signupHandler) "user1" (PlainPassword "password1")
        logs `shouldSatisfy` (== 0)
          `times` call anything
      it "does not call `upsertPasswordAuth`" $ do
        logs <- runRIO (env pool) $
          withMonitor_ $ \monitor ->
            local (authRepository . upsertPasswordAuth %~ watch monitor) $
              void $ invoke (authHandlerV . signupHandler) "user1" (PlainPassword "password1")
        logs `shouldSatisfy` (== 0)
          `times` call anything
