{-# LANGUAGE OverloadedStrings #-}

module Housekeeping.Service.Hello.Repository
  ( helloRepositoryImpl,
  )
where

import Control.Env.Hierarchical
import Housekeeping.DataSource
  ( Database,
    Only (Only, fromOnly),
    execute,
    query_,
  )
import Housekeeping.Service.Hello.Handler
import RIO

selectMessageImpl :: Has1 Database env => RIO env [Text]
selectMessageImpl =
  map fromOnly
    <$> runIF (\db -> view query_ db "SELECT msg FROM message")

insertMessageImpl :: Has1 Database env => Text -> RIO env ()
insertMessageImpl msg =
  void $
    runIF $ \db ->
      view
        execute
        db
        "INSERT INTO message (msg) VALUES (?)"
        (Only msg)

helloRepositoryImpl :: Has1 Database env => HelloRepository env
helloRepositoryImpl =
  HelloRepository
    { _insertMessage = insertMessageImpl,
      _selectMessage = selectMessageImpl
    }