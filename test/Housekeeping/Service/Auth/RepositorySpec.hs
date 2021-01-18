module Housekeeping.Service.Auth.RepositorySpec where

import Control.Method
import Housekeeping.DataSource
import Housekeeping.Service.Auth.Interface
import Housekeeping.Service.Auth.Model
import Housekeeping.Service.Auth.Repository
import Housekeeping.TestHelper
import Lens.Micro.Platform
import RIO (runRIO)
import Test.Hspec
import Test.Method

newtype Env = Env {_database :: Database Env}

makeLenses ''Env

instance ViewDatabase Env where
  databaseV = database

instance ViewUserRepository Env where
  userRepositoryV = to $ const userRepositoryImpl

databaseMock :: Database Env
databaseMock =
  Database
    { _query = mockup $ do
        let sql = "SELECT user_name, user_id FROM user WHERE user_name = ?"
        when (args ((== sql), dynEq (Only ("user1" :: UserName))))
          `thenReturn` [dyn (User "user1" 0)]
        when (args ((== sql), dynEq (Only ("same_user" :: UserName))))
          `thenReturn` [dyn (User "same_user" 1), dyn (User "same_user" 2)]
        when (args ((== sql), anything))
          `thenReturn` [],
      _query_ = undefined,
      _execute = undefined,
      _execute_ = undefined
    }

spec :: Spec
spec = do
  describe "userRepositoryImpl" $ do
    describe "findUserByUserName" $ do
      context "when username is registered" $ do
        it "return Just user" $ do
          mUser <- runRIO (Env databaseMock) $ do
            invoke (userRepositoryV . findUserByUserName) "user1"
          mUser `shouldBe` Just (User "user1" 0)
      context "when username is not registered" $ do
        it "return Nothing" $ do
          mUser <- runRIO (Env databaseMock) $ do
            invoke (userRepositoryV . findUserByUserName) "user2"
          mUser `shouldBe` Nothing
      context "when multiple user has same user_name" $ do
        it "throws error" $
          runRIO
            (Env databaseMock)
            (invoke (userRepositoryV . findUserByUserName) "same_user")
            `shouldThrow` anyErrorCall
