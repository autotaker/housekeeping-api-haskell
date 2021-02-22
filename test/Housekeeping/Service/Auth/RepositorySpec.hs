{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Housekeeping.Service.Auth.RepositorySpec where

import Control.Method
import Control.Method.Internal
import qualified Crypto.BCrypt as BCrypt
import Data.Maybe (fromJust)
import Database.PostgreSQL.Simple (Query)
import Housekeeping.DataSource
import Housekeeping.Service.Auth.Interface
import Housekeeping.Service.Auth.Model
import Housekeeping.Service.Auth.Repository
import Lens.Micro.Platform
import RIO (ByteString, Int64, MonadReader (local), RIO, runRIO)
import Test.Hspec
import Test.Method

newtype Env = Env
  {_database :: Database Env}

makeLenses ''Env

instance ViewDatabase Env where
  databaseV = database

instance HasDatabase Env where
  databaseL = database

instance ViewUserRepository Env where
  userRepositoryV = to $ const userRepositoryImpl

instance ViewAuthRepository Env where
  authRepositoryV = to $ const authRepositoryImpl

user1 :: User
user1 = User "user1" 0

databaseMock :: Database Env
databaseMock =
  Database
    { _query = castMethod queryDyn,
      _query_ = undefined,
      _execute = mockup $ do
        when anything `thenReturn` 0,
      _execute_ = undefined,
      _returning = undefined
    }
  where
    queryDyn :: Query -> Dynamic -> RIO Env [Dynamic]
    queryDyn = mockup $ do
      let sql = "SELECT user_name, user_id FROM user WHERE user_name = ?"
      when (args ((== sql), dynArg (== Only ("user1" :: UserName))))
        `thenReturn` toDyn [user1]
      when (args ((== sql), dynArg (== Only ("same_user" :: UserName))))
        `thenReturn` toDyn [User "same_user" 1, User "same_user" 2]
      when (args ((== sql), anything))
        `thenReturn` []
      let createUserSql = "INSERT INTO user (user_name) VALUES (?) RETURNING user_id"
      when (args ((== createUserSql), dynArg (== Only ("user2" :: UserName))))
        `thenReturn` toDyn [Only (2 :: Int)]
      when (args ((== createUserSql), dynArg (== Only ("broken_user" :: UserName))))
        `thenReturn` []

      let selectPasswordSql =
            "SELECT u.user_id, u.user_name, a.hashed_password FROM"
              <> " user u INNER JOIN auth_password a"
              <> " ON u.user_id = a.user_id"
              <> " AND u.user_name = ?"
      when (args ((== selectPasswordSql), dynArg (== Only ("user1" :: UserName))))
        `thenReturn` toDyn [user1 :. Only password1]
      when (args ((== selectPasswordSql), dynArg (== Only ("user2" :: UserName))))
        `thenReturn` []
      when (args ((== selectPasswordSql), dynArg (== Only ("same_user" :: UserName))))
        `thenReturn` toDyn [User "same_user" 1 :. Only password1, User "same_user" 1 :. Only password1]

env :: Env
env = Env databaseMock

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
    describe "upsertPasswordAuth" $ do
      it "execute upsert query" $ do
        let auth = PasswordAuth user1 password1
        logs <- runRIO env $
          withMonitor_ $ \monitor ->
            local (databaseL %~ (\db -> db {_execute = watchBy toDyn id monitor $ _execute db})) $ do
              invoke (authRepositoryV . upsertPasswordAuth) auth
        (logs :: [Event (Query :* Dynamic :* Nil) Int64])
          `shouldSatisfy` (== 1)
            `times` call (args (anything, dynArg (== (0 :: UserId, password1))))

password1 :: HashedPassword
password1 = HashedPassword $ fromJust $ BCrypt.hashPassword "password1" mockSalt
