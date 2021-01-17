{-# LANGUAGE TypeOperators #-}

module Housekeeping.Service.Hello.RepositorySpec where

import Control.Method (TupleLike (toTuple), invoke)
import Data.Dynamic
import Data.Maybe (fromJust)
import Data.Typeable (cast)
import Database.PostgreSQL.Simple (Query)
import Housekeeping.DataSource
import Housekeeping.Service.Hello.Interface
import Housekeeping.Service.Hello.Repository
import Lens.Micro.Platform (makeLenses, to, (%~))
import RIO (MonadReader (local), Text, runRIO)
import Test.Hspec
import Test.Method (ArgsMatcher (args), Event, Matcher, anything, call, mockup, thenReturn, times, watchBy, when, withMonitor_)

dyn :: (Typeable a, Typeable b) => a -> b
dyn a = fromJust $ cast a

databaseMock :: Database env
databaseMock =
  Database
    { _query_ = mockup $ do
        when (args (== "SELECT msg FROM message"))
          `thenReturn` dyn [Only ("hello world!" :: Text)],
      _query = undefined,
      _execute = mockup $ do
        when (args ((== "INSERT INTO message (msg) VALUES (?)"), anything))
          `thenReturn` 0,
      _execute_ = undefined
    }

newtype Env = Env {_database :: Database Env}

makeLenses ''Env

instance ViewDatabase Env where
  databaseV = database

instance HasDatabase Env where
  databaseL = database

instance ViewHelloRepository Env where
  helloRepositoryV = to $ const helloRepositoryImpl

dynEq :: (Typeable a, Eq a) => a -> Dynamic -> Bool
dynEq a dVal = fromDynamic dVal == Just a

callDyn :: Typeable a => Matcher a -> Matcher (Event Dynamic b)
callDyn matcher =
  call $ \dVal -> Just True == (matcher <$> fromDynamic dVal)

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
          local (databaseL %~ (\db -> db {_execute = watchBy (toDyn . toTuple) id monitor (_execute db)})) $
            invoke (helloRepositoryV . insertMessage) "hello world!"
      logs `shouldSatisfy` (== 1)
        `times` callDyn ((== Only "hello world!") . snd :: Matcher (Query, Only Text))
