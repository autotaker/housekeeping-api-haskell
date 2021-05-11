{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Housekeeping.Service.Hello.Repository
  ( helloRepositoryImpl,
  )
where

import Control.Env.Hierarchical
import Housekeeping.DataSource
  ( Database (..),
    Only (Only, fromOnly),
    execute,
    query_,
  )
import Housekeeping.Service.Hello.Handler
import RIO

selectMessageImpl :: Has1 Database env => RIO env [Text]
selectMessageImpl = runIF $ \Database {..} ->
  map fromOnly <$> query_ "SELECT msg FROM message"

insertMessageImpl :: Has1 Database env => Text -> RIO env ()
insertMessageImpl msg = void $
  runIF $ \Database {..} ->
    execute
      "INSERT INTO message (msg) VALUES (?)"
      (Only msg)

helloRepositoryImpl :: Has1 Database env => HelloRepository env
helloRepositoryImpl =
  HelloRepository
    { insertMessage = insertMessageImpl,
      selectMessage = selectMessageImpl
    }