{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Housekeeping.Service.Hello.RepositorySpec where

import Control.Method (Method (Args), invoke)
import Data.Dynamic (toDyn)
import Database.PostgreSQL.Simple (Query)
import Housekeeping.DataSource
  ( Database (..),
    HasDatabase (..),
    Only (Only),
    ViewDatabase (..),
  )
import Housekeeping.Service.Hello.Interface
  ( ViewHelloRepository (..),
    insertMessage,
    selectMessage,
  )
import Housekeeping.Service.Hello.Repository
  ( helloRepositoryImpl,
  )
import Housekeeping.TestHelper (callDyn, dyn)
import Lens.Micro.Platform (makeLenses, to, (%~))
import RIO (MonadReader (local), RIO, Text, runRIO)
import Test.Hspec (Spec, describe, it, shouldBe, shouldSatisfy)
import Test.Method (ArgsMatcher (args), anything, mockup, thenReturn, times, watchBy, when, withMonitor_)

databaseMock :: Database env
databaseMock =
  Database
    { _query_ = mockup $ do
        when (args (== "SELECT msg FROM message"))
          `thenReturn` [dyn $ Only ("hello world!" :: Text)],
      _query = undefined,
      _execute = mockup $ do
        when (args ((== "INSERT INTO message (msg) VALUES (?)"), anything))
          `thenReturn` 0,
      _execute_ = undefined,
      _returning = undefined
    }

newtype Env = Env {_database :: Database Env}

makeLenses ''Env

instance ViewDatabase Env where
  databaseV = database

instance HasDatabase Env where
  databaseL = database

instance ViewHelloRepository Env where
  helloRepositoryV = to $ const helloRepositoryImpl

spec :: Spec
spec = do
  describe "selectMessage" $ do
    it "query select message" $ do
      r <- runRIO (Env databaseMock) $ do
        invoke (helloRepositoryV . selectMessage)
      r `shouldBe` ["hello world!"]

  describe "insertMessage" $ do
    it "query insert message" $ do
      logs <- runRIO (Env databaseMock) $
        withMonitor_ $ \monitor -> do
          local (databaseL %~ (\db -> db {_execute = watchBy toDyn id monitor (_execute db)})) $
            invoke (helloRepositoryV . insertMessage) "hello world!"
      logs `shouldSatisfy` (== 1)
        `times` callDyn @(Args (Query -> Only Text -> RIO Env ()))
          (args (anything, (== Only "hello world!")))
