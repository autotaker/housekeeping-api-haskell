{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Housekeeping.Service.Hello.RepositorySpec where

import Control.Env.Hierarchical (deriveEnv)
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
import Lens.Micro.Platform (view)
import RIO (Text, runRIO)
import Test.Hspec (Spec, describe, it, shouldReturn)
import Test.Method (ToDyn (toDyn), anything, decl, deriveLabel, dynArg, thenReturn, whenArgs, withProtocol)

newtype Env = Env (Database Env)

deriveEnv ''Env
deriveLabel ''Database

spec :: Spec
spec = do
  describe "selectMessage" $ do
    it "query select message" $ do
      let proto =
            decl $
              whenArgs Query_ anything
                `thenReturn` toDyn [Only ("hello world!" :: Text)]
      withProtocol proto $ \db ->
        runRIO (Env db) (view selectMessage helloRepositoryImpl)
          `shouldReturn` ["hello world!"]

  describe "insertMessage" $ do
    it "query insert message" $ do
      let proto =
            decl $
              whenArgs Execute (anything, dynArg (== Only ("hello world!" :: Text)))
                `thenReturn` 1
      withProtocol proto $ \db ->
        runRIO (Env db) (view insertMessage helloRepositoryImpl "hello world!")
          `shouldReturn` ()
