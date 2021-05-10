{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Housekeeping.Service.Auth.RepositorySpec (spec) where

import Control.Env.Hierarchical
import qualified Crypto.BCrypt as BCrypt
import Data.Maybe (fromJust)
import Housekeeping.DataSource
import Housekeeping.Service.Auth.Interface
import Housekeeping.Service.Auth.Model
import Housekeeping.Service.Auth.Repository
import Lens.Micro.Platform
import RIO (ByteString, runRIO)
import Test.Hspec
import Test.Method

newtype Env = Env (Database Env)

deriveEnv ''Env

deriveLabel ''Database

user1 :: User
user1 = User "user1" 0

mockSalt :: ByteString
mockSalt = "$2y$04$akDsXE7raEDxa1btakPxWO"

password1 :: HashedPassword
password1 = HashedPassword $ fromJust $ BCrypt.hashPassword "password1" mockSalt

spec :: Spec
spec = do
  describe "userRepositoryImpl" $ do
    describe "findUserByUserName" $ do
      let run :: ProtocolM (DatabaseLabel Env) a -> IO (Maybe User)
          run proto =
            withProtocol proto $ \db ->
              runRIO (Env db) $
                view findUserByUserName userRepositoryImpl usernm
          usernm :: UserName
          usernm = "user1"
      context "when username is registered" $ do
        it "return Just user" $ do
          let proto =
                decl $
                  whenArgs Query (anything, dynArg (== Only usernm))
                    `thenReturn` toDyn [user1]
          run proto `shouldReturn` Just (User "user1" 0)
      context "when username is not registered" $ do
        it "return Nothing" $ do
          let proto =
                decl $
                  whenArgs Query (anything, dynArg (== Only usernm))
                    `thenReturn` toDyn ([] :: [User])
          run proto `shouldReturn` Nothing
      context "when multiple user has same user_name" $ do
        it "throws error" $ do
          let proto =
                decl $
                  whenArgs Query (anything, dynArg (== Only usernm))
                    `thenReturn` toDyn [user1, user1]
          run proto `shouldThrow` anyErrorCall

    describe "createUser" $ do
      let run :: ProtocolM (DatabaseLabel Env) a -> IO User
          run proto =
            withProtocol proto $ \db ->
              runRIO (Env db) $
                view createUser userRepositoryImpl user
          user = User "user1" 0
          usernm :: UserName
          usernm = "user1"
      it "return User with fresh user_id" $ do
        let proto =
              decl $
                whenArgs Query (anything, dynArg (== Only usernm))
                  `thenReturn` toDyn [Only (1 :: Int)]
        run proto `shouldReturn` User "user1" 1
      context "if database returns non-single rows" $ do
        let proto =
              decl $
                whenArgs Query (anything, dynArg (== Only usernm))
                  `thenReturn` toDyn [Only (1 :: Int), Only (2 :: Int)]
        it "throw error" $
          run proto `shouldThrow` anyErrorCall

  describe "authRepositoryImpl" $ do
    describe "findPasswordAuthByUserName" $ do
      let run :: ProtocolM (DatabaseLabel Env) a -> IO (Maybe PasswordAuth)
          run proto =
            withProtocol proto $ \db ->
              runRIO (Env db) $
                view findPasswordAuthByUserName authRepositoryImpl usernm
          usernm :: UserName
          usernm = "user1"
      context "when password is registered" $ do
        it "returns Just passwordAuth" $ do
          let proto =
                decl $
                  whenArgs Query (anything, dynArg (== Only usernm))
                    `thenReturn` toDyn [user1 :. Only password1]
          run proto `shouldReturn` Just (PasswordAuth user1 password1)
      context "when password is not registered" $ do
        it "returns Nothing" $ do
          let proto =
                decl $
                  whenArgs Query (anything, dynArg (== Only usernm))
                    `thenReturn` toDyn ([] :: [PasswordAuth])
          run proto `shouldReturn` Nothing
      context "when multiple passwords are registered" $ do
        it "throws error" $ do
          let proto =
                decl $
                  whenArgs Query (anything, dynArg (== Only usernm))
                    `thenReturn` toDyn [user1 :. Only password1, user1 :. Only password1]
          run proto `shouldThrow` anyErrorCall
    describe "upsertPasswordAuth" $ do
      it "execute upsert query" $ do
        let auth = PasswordAuth user1 password1
            proto = do
              decl $
                whenArgs Execute (anything, dynArg (== (0 :: UserId, password1)))
                  `thenReturn` 1
        withProtocol proto $ \db ->
          runRIO (Env db) (view upsertPasswordAuth authRepositoryImpl auth)
            `shouldReturn` ()
