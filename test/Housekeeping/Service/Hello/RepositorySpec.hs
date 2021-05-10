{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Housekeeping.Service.Hello.RepositorySpec where

import Control.Env.Hierarchical
import Control.Method (Method (Args))
import Database.PostgreSQL.Simple (Query)
import Housekeeping.DataSource
  ( Database (..),
    Only (Only),
  )
import Housekeeping.Service.Hello.Interface
  ( insertMessage,
    selectMessage,
  )
import Housekeeping.Service.Hello.Repository
  ( helloRepositoryImpl,
  )
import Lens.Micro.Platform (view, (%~))
import RIO (Int64, MonadReader (local), RIO, Text, runRIO)
import Test.Hspec (Spec, describe, it, shouldBe, shouldSatisfy)
import Test.Method (ArgsMatcher (args), Dynamic, Event, ToDyn (toDyn), anything, call, castMethod, dynArg, mockup, thenReturn, times, watchBy, when, withMonitor_)

databaseMock :: Database env
databaseMock =
  Database
    { _query_ = castMethod queryDyn,
      _query = undefined,
      _execute = mockup $ do
        when (args ((== "INSERT INTO message (msg) VALUES (?)"), anything))
          `thenReturn` 0,
      _execute_ = undefined,
      _returning = undefined
    }
  where
    queryDyn :: Query -> RIO env [Dynamic]
    queryDyn = mockup $ do
      when (args (== "SELECT msg FROM message"))
        `thenReturn` toDyn [Only ("hello world!" :: Text)]

newtype Env = Env (Database Env)

deriveEnv ''Env

spec :: Spec
spec = do
  describe "selectMessage" $ do
    it "query select message" $ do
      r <- runRIO (Env databaseMock) $ do
        view selectMessage helloRepositoryImpl
      r `shouldBe` ["hello world!"]

  describe "insertMessage" $ do
    it "query insert message" $ do
      logs <- runRIO (Env databaseMock) $
        withMonitor_ $ \monitor -> do
          local (getL @(Database Env) %~ (\db -> db {_execute = watchBy toDyn id monitor (_execute db)})) $
            view insertMessage helloRepositoryImpl "hello world!"
      (logs :: [Event (Args (Query -> Dynamic -> RIO Env Int64)) Int64]) `shouldSatisfy` (== 1)
        `times` call (args (anything, dynArg (== Only ("hello world!" :: Text))))
