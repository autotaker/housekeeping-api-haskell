module Housekeeping.Service.Auth.RepositorySpec where

import Control.Method
import qualified Crypto.BCrypt as BCrypt
import Data.Maybe (fromJust)
import Housekeeping.DataSource
import Housekeeping.Service.Auth.Interface
import Housekeeping.Service.Auth.Model
import Housekeeping.Service.Auth.Repository
import Housekeeping.TestHelper
import Lens.Micro.Platform
import RIO (ByteString, runRIO)
import Test.Hspec
import Test.Method

data Env = Env
  { _database :: Database Env,
    _passwordHasher :: PasswordHasher Env
  }

makeLenses ''Env

instance ViewDatabase Env where
  databaseV = database

instance ViewUserRepository Env where
  userRepositoryV = to $ const userRepositoryImpl

instance ViewAuthRepository Env where
  authRepositoryV = to $ const authRepositoryImpl

instance ViewPasswordHasher Env where
  passwordHasherV = passwordHasher

user1 :: User
user1 = User "user1" 0

databaseMock :: Database Env
databaseMock =
  Database
    { _query = mockup $ do
        let sql = "SELECT user_name, user_id FROM user WHERE user_name = ?"
        when (args ((== sql), dynEq (Only ("user1" :: UserName))))
          `thenReturn` [dyn user1]
        when (args ((== sql), dynEq (Only ("same_user" :: UserName))))
          `thenReturn` [dyn (User "same_user" 1), dyn (User "same_user" 2)]
        when (args ((== sql), anything))
          `thenReturn` []
        let createUserSql = "INSERT INTO user (user_name) VALUES (?) RETURNING user_id"
        when (args ((== createUserSql), dynEq (Only ("user2" :: UserName))))
          `thenReturn` [dyn $ Only (2 :: Int)]
        when (args ((== createUserSql), dynEq (Only ("broken_user" :: UserName))))
          `thenReturn` []

        let selectPasswordSql =
              "SELECT u.user_id, u.user_name, a.hashed_password FROM"
                <> " user u INNER JOIN auth_password a"
                <> " ON u.user_id = a.user_id"
                <> " AND u.user_name = ?"
        when (args ((== selectPasswordSql), dynEq (Only ("user1" :: UserName))))
          `thenReturn` [dyn $ user1 :. Only password1]
        when (args ((== selectPasswordSql), dynEq (Only ("user2" :: UserName))))
          `thenReturn` []
        when (args ((== selectPasswordSql), dynEq (Only ("same_user" :: UserName))))
          `thenReturn` [dyn (User "same_user" 1 :. Only password1), dyn (User "same_user" 1 :. Only password1)],
      _query_ = undefined,
      _execute = undefined,
      _execute_ = undefined,
      _returning = undefined
    }

passwordHasherMock :: PasswordHasher Env
passwordHasherMock =
  PasswordHasher
    { _hashPassword = \(PlainPassword password) ->
        pure $ HashedPassword $ fromJust $ BCrypt.hashPassword password mockSalt
    }

env :: Env
env = Env databaseMock passwordHasherMock

mockSalt :: ByteString
mockSalt = "$2y$04$akDsXE7raEDxa1btakPxWO"

spec :: Spec
spec = do
  describe "userRepositoryImpl" $ do
    describe "findUserByUserName" $ do
      let run username =
            runRIO env $
              invoke (userRepositoryV . findUserByUserName) username
      context "when username is registered" $ do
        it "return Just user" $ do
          run "user1" `shouldReturn` Just (User "user1" 0)
      context "when username is not registered" $ do
        it "return Nothing" $ do
          run "user2" `shouldReturn` Nothing
      context "when multiple user has same user_name" $ do
        it "throws error" $
          run "same_user"
            `shouldThrow` anyErrorCall

    describe "createUser" $ do
      let run user = runRIO env $ invoke (userRepositoryV . createUser) user
      it "return User with fresh user_id" $ do
        run (User "user2" 0) `shouldReturn` User "user2" 2
      context "if database returns non-single rows" $ do
        it "throw error" $
          run (User "broken_user" 0) `shouldThrow` anyErrorCall

  describe "authRepositoryImpl" $ do
    describe "findPasswordAuthByUserName" $ do
      let run username =
            runRIO env $
              invoke (authRepositoryV . findPasswordAuthByUserName) username
      context "when password is registered" $ do
        it "returns Just passwordAuth" $
          run "user1" `shouldReturn` Just (PasswordAuth user1 password1)
      context "when password is not registered" $ do
        it "returns Nothing" $
          run "user2" `shouldReturn` Nothing
      context "when multiple passwords are registered" $ do
        it "throws error" $
          run "same_user" `shouldThrow` anyErrorCall

password1 :: HashedPassword
password1 = HashedPassword $ fromJust $ BCrypt.hashPassword "password1" mockSalt
