{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Housekeeping.Service.Auth.HandlerSpec (spec) where

import Control.Env.Hierarchical
import qualified Crypto.BCrypt as BCrypt
import Data.Maybe (fromJust)
import Data.Pool (Pool, createPool)
import Housekeeping.DataSource
  ( IConnection,
    TransactionManager,
    Transactional (..),
    defaultTransactionManager,
  )
import Housekeeping.Service.Auth.Handler (authHandlerImpl)
import Housekeeping.Service.Auth.Interface
  ( AuthRepository (..),
    PasswordHasher (..),
    UserRepository (..),
    signinHandler,
    signupHandler,
  )
import Housekeeping.Service.Auth.Model
  ( HashedPassword (..),
    PasswordAuth (..),
    PlainPassword (..),
  )
import Housekeeping.Session (User (User, _userId, _userName), userName)
import Lens.Micro.Platform ((^.))
import RIO
  ( ByteString,
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
    deriveLabel,
    mockInterface,
    protocol,
    thenAction,
    thenReturn,
    verify,
    whenArgs,
    (:|:) (L, R),
  )

data Env
  = Env
      (UserRepository Env)
      (AuthRepository Env)
      (PasswordHasher Env)
      (TransactionManager MConnection)
      (Pool MConnection)

data MConnection = MConnection
  { mBegin :: IO (),
    mCommit :: IO (),
    mRollback :: IO ()
  }

instance Transactional MConnection where
  begin = mBegin
  commit = mCommit
  rollback = mRollback

deriveEnv ''Env

deriveLabel ''UserRepository

deriveLabel ''AuthRepository

deriveLabel ''PasswordHasher

deriveLabel ''MConnection

mockSalt :: ByteString
mockSalt = "$2y$04$akDsXE7raEDxa1btakPxWO"

password1 :: PlainPassword
password1 = PlainPassword "password1"

hashedPassword1 :: HashedPassword
hashedPassword1 = HashedPassword $ fromJust $ BCrypt.hashPassword "password1" mockSalt

passwordHasherMock :: PasswordHasher env
passwordHasherMock =
  PasswordHasher
    { hashPassword = \(PlainPassword passwd) ->
        pure $ HashedPassword $ fromJust $ BCrypt.hashPassword passwd mockSalt
    }

auth1 :: PasswordAuth
auth1 = PasswordAuth user1 hashedPassword1

user1 :: User
user1 = User {_userName = "user1", _userId = 0}

type instance IConnection Env = MConnection

type Methods = AuthRepositoryLabel Env :|: UserRepositoryLabel Env :|: MConnectionLabel

class Inj f where
  inj :: f m -> Methods m

instance env ~ Env => Inj (AuthRepositoryLabel env) where
  inj = L . L

instance env ~ Env => Inj (UserRepositoryLabel env) where
  inj = L . R

instance Inj MConnectionLabel where
  inj = R

mkEnv :: ProtocolEnv Methods -> IO Env
mkEnv penv = do
  let ((authRepo, userRepo), conn) = mockInterface penv

  pool <- createPool (pure conn) (const $ pure ()) 1 100 1
  transactionManager <- defaultTransactionManager
  pure $
    Env
      userRepo
      authRepo
      passwordHasherMock
      transactionManager
      pool

spec :: Spec
spec = do
  describe "signupHandler" $ do
    let usernm = user1 ^. userName
    context "when the username is not registered yet" $ do
      it "returns `Just user` and commits the transaction" $ do
        penv <- protocol $ do
          findUserCall <- decl $ whenArgs (inj FindUserByUserName) (== usernm) `thenReturn` Nothing
          beginCall <- decl $ whenArgs (inj MBegin) () `thenReturn` ()
          createUserCall <-
            decl $
              whenArgs (inj CreateUser) (== user1 {_userId = -1})
                `thenReturn` user1
                `dependsOn` [findUserCall, beginCall]
          upsertPasswordAuthCall <-
            decl $
              whenArgs (inj UpsertPasswordAuth) (== auth1)
                `thenReturn` () `dependsOn` [createUserCall]
          decl $ whenArgs (inj MCommit) () `thenReturn` () `dependsOn` [upsertPasswordAuthCall]
        env <- mkEnv penv
        runRIO env (signupHandler authHandlerImpl usernm password1) `shouldReturn` Just user1
        verify penv
    context "when the username is already registered" $ do
      it "returns `Nothing` and doesn't begin a transaction" $ do
        penv <- protocol $ do
          decl $ whenArgs (inj FindUserByUserName) (== usernm) `thenReturn` Just user1
        env <- mkEnv penv
        runRIO env (signupHandler authHandlerImpl usernm password1) `shouldReturn` Nothing
        verify penv
    context "when exception raised during transaction" $ do
      it "raises the exception and rollback the transaction" $ do
        penv <- protocol $ do
          findUserCall <- decl $ whenArgs (inj FindUserByUserName) (== usernm) `thenReturn` Nothing
          beginCall <- decl $ whenArgs (inj MBegin) () `thenReturn` ()
          createUserCall <-
            decl $
              whenArgs (inj CreateUser) (== user1 {_userId = -1})
                `thenAction` throwString "failed to create user"
                `dependsOn` [findUserCall, beginCall]
          decl $ whenArgs (inj MRollback) () `thenReturn` () `dependsOn` [createUserCall]
        env <- mkEnv penv
        let anyStringException :: StringException -> Bool
            anyStringException _ = True
        runRIO env (signupHandler authHandlerImpl usernm password1) `shouldThrow` anyStringException
        verify penv
  describe "signinHandler" $ do
    let usernm = "username1"
        wrongPasswd = PlainPassword "wrong password"
    context "when both username and password are correct" $ do
      it "returns `Authenticated user`" $ do
        penv <- protocol $ do
          decl $
            whenArgs (inj FindPasswordAuthByUserName) (== usernm)
              `thenReturn` Just auth1
        env <- mkEnv penv
        runRIO env (signinHandler authHandlerImpl usernm password1) `shouldReturn` Authenticated user1
    context "when auth is not registered for given username" $ do
      it "returns `NoSuchUser`" $ do
        penv <- protocol $ do
          decl $
            whenArgs (inj FindPasswordAuthByUserName) (== usernm)
              `thenReturn` Nothing
        env <- mkEnv penv
        runRIO env (signinHandler authHandlerImpl usernm password1) `shouldReturn` NoSuchUser
    context "when password is wrong" $ do
      it "returns `BadPassword`" $ do
        penv <- protocol $ do
          decl $
            whenArgs (inj FindPasswordAuthByUserName) (== usernm)
              `thenReturn` Just auth1
        env <- mkEnv penv
        runRIO env (signinHandler authHandlerImpl usernm wrongPasswd) `shouldReturn` BadPassword