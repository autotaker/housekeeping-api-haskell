{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}

module Housekeeping.Service.Auth.HandlerSpec (spec) where

import Control.Method (invoke)
import qualified Crypto.BCrypt as BCrypt
import Data.Maybe (fromJust)
import Data.Pool (Pool, createPool)
import Housekeeping.DataSource
  ( HasConnectionPool (..),
    HasTransactionManager (..),
    TransactionManager,
    Transactional (..),
    defaultTransactionManager,
  )
import Housekeeping.Service.Auth.Handler (authHandlerImpl)
import Housekeeping.Service.Auth.Interface
  ( AuthRepository (..),
    PasswordHasher (..),
    UserRepository (..),
    ViewAuthHandler (..),
    ViewAuthRepository (..),
    ViewPasswordHasher (..),
    ViewUserRepository (..),
    signinHandler,
    signupHandler,
  )
import Housekeeping.Service.Auth.Model
  ( HashedPassword (..),
    PasswordAuth (..),
    PlainPassword (..),
    User (..),
    UserName,
    userName,
  )
import Lens.Micro.Platform (makeLenses, to, (^.))
import RIO
  ( ByteString,
    RIO,
    StringException,
    runRIO,
    throwString,
  )
import Servant.Auth.Server
  ( AuthResult (Authenticated, BadPassword, NoSuchUser),
  )
import Test.Hspec
  ( Spec,
    context,
    describe,
    it,
    shouldReturn,
    shouldThrow,
  )
import Test.Method
  ( ProtocolEnv,
    decl,
    dependsOn,
    lookupMock,
    protocol,
    thenAction,
    thenReturn,
    verify,
    whenArgs,
  )

data Env = Env
  { _userRepository :: UserRepository Env,
    _authRepository :: AuthRepository Env,
    _passwordHasher :: PasswordHasher Env,
    _transactionManager :: TransactionManager MConnection,
    _connectionPool :: Pool MConnection
  }

data MConnection = MConnection
  { mBegin :: IO (),
    mCommit :: IO (),
    mRollback :: IO ()
  }

instance Transactional MConnection where
  begin = mBegin
  commit = mCommit
  rollback = mRollback

makeLenses ''Env

data Methods m where
  HashPassword :: Methods (PlainPassword -> RIO Env HashedPassword)
  FindUserByUserName :: Methods (UserName -> RIO Env (Maybe User))
  CreateUser :: Methods (User -> RIO Env User)
  FindPasswordAuthByUserName :: Methods (UserName -> RIO Env (Maybe PasswordAuth))
  UpsertPasswordAuth :: Methods (PasswordAuth -> RIO Env ())
  TBegin :: Methods (IO ())
  TCommit :: Methods (IO ())
  TRollback :: Methods (IO ())

deriving instance Eq (Methods m)

deriving instance Ord (Methods m)

deriving instance Show (Methods m)

mockSalt :: ByteString
mockSalt = "$2y$04$akDsXE7raEDxa1btakPxWO"

password1 :: PlainPassword
password1 = PlainPassword "password1"

hashedPassword1 :: HashedPassword
hashedPassword1 = HashedPassword $ fromJust $ BCrypt.hashPassword "password1" mockSalt

auth1 :: PasswordAuth
auth1 = PasswordAuth user1 hashedPassword1

user1 :: User
user1 = User {_userName = "user1", _userId = 0}

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

instance HasConnectionPool Env where
  type IConnection Env = MConnection
  connectionPoolL = connectionPool

mkEnv :: ProtocolEnv Methods -> IO Env
mkEnv penv = do
  let conn =
        MConnection
          { mBegin = lookupMock TBegin penv,
            mCommit = lookupMock TCommit penv,
            mRollback = lookupMock TRollback penv
          }
  pool <- createPool (pure conn) (const $ pure ()) 1 100 1
  pure
    Env
      { _authRepository =
          AuthRepository
            { _findPasswordAuthByUserName = lookupMock FindPasswordAuthByUserName penv,
              _upsertPasswordAuth = lookupMock UpsertPasswordAuth penv
            },
        _userRepository =
          UserRepository
            { _createUser = lookupMock CreateUser penv,
              _findUserByUserName = lookupMock FindUserByUserName penv
            },
        _passwordHasher =
          PasswordHasher
            { _hashPassword = lookupMock HashPassword penv
            },
        _transactionManager = defaultTransactionManager,
        _connectionPool = pool
      }

spec :: Spec
spec = do
  describe "signupHandler" $ do
    let usernm = user1 ^. userName
    context "when the username is not registered yet" $ do
      it "returns `Just user` and commits the transaction" $ do
        penv <- protocol $ do
          findUserCall <- decl $ whenArgs FindUserByUserName (== usernm) `thenReturn` Nothing
          beginCall <- decl $ whenArgs TBegin () `thenReturn` ()
          createUserCall <-
            decl $
              whenArgs CreateUser (== user1 {_userId = -1})
                `thenReturn` user1
                `dependsOn` [findUserCall, beginCall]
          hashPasswordCall <- decl $ whenArgs HashPassword (== password1) `thenReturn` hashedPassword1
          upsertPasswordAuthCall <-
            decl $
              whenArgs UpsertPasswordAuth (== auth1)
                `thenReturn` () `dependsOn` [createUserCall, hashPasswordCall]
          decl $ whenArgs TCommit () `thenReturn` () `dependsOn` [upsertPasswordAuthCall]
        env <- mkEnv penv
        runRIO env (invoke (authHandlerV . signupHandler) usernm password1) `shouldReturn` Just user1
        verify penv
    context "when the username is already registered" $ do
      it "returns `Nothing` and doesn't begin a transaction" $ do
        penv <- protocol $ do
          decl $ whenArgs FindUserByUserName (== usernm) `thenReturn` Just user1
        env <- mkEnv penv
        runRIO env (invoke (authHandlerV . signupHandler) usernm password1) `shouldReturn` Nothing
        verify penv
    context "when exception raised during transaction" $ do
      it "raises the exception and rollback the transaction" $ do
        penv <- protocol $ do
          findUserCall <- decl $ whenArgs FindUserByUserName (== usernm) `thenReturn` Nothing
          beginCall <- decl $ whenArgs TBegin () `thenReturn` ()
          createUserCall <-
            decl $
              whenArgs CreateUser (== user1 {_userId = -1})
                `thenAction` throwString "failed to create user"
                `dependsOn` [findUserCall, beginCall]
          decl $ whenArgs TRollback () `thenReturn` () `dependsOn` [createUserCall]
        env <- mkEnv penv
        let anyStringException :: StringException -> Bool
            anyStringException _ = True
        runRIO env (invoke (authHandlerV . signupHandler) usernm password1) `shouldThrow` anyStringException
        verify penv
  describe "signinHandler" $ do
    let usernm = "username1"
        wrongPasswd = PlainPassword "wrong password"
    context "when both username and password are correct" $ do
      it "returns `Authenticated user`" $ do
        penv <- protocol $ do
          decl $
            whenArgs FindPasswordAuthByUserName (== usernm)
              `thenReturn` Just auth1
        env <- mkEnv penv
        runRIO env (invoke (authHandlerV . signinHandler) usernm password1) `shouldReturn` Authenticated user1
    context "when auth is not registered for given username" $ do
      it "returns `NoSuchUser`" $ do
        penv <- protocol $ do
          decl $
            whenArgs FindPasswordAuthByUserName (== usernm)
              `thenReturn` Nothing
        env <- mkEnv penv
        runRIO env (invoke (authHandlerV . signinHandler) usernm password1) `shouldReturn` NoSuchUser
    context "when password is wrong" $ do
      it "returns `BadPassword`" $ do
        penv <- protocol $ do
          decl $
            whenArgs FindPasswordAuthByUserName (== usernm)
              `thenReturn` Just auth1
        env <- mkEnv penv
        runRIO env (invoke (authHandlerV . signinHandler) usernm wrongPasswd) `shouldReturn` BadPassword