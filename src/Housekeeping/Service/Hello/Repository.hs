{-# LANGUAGE OverloadedStrings #-}

module Housekeeping.Service.Hello.Repository
  ( helloRepositoryImpl,
  )
where

import Control.Method (invoke)
import Housekeeping.DataSource
  ( Only (Only, fromOnly),
    ViewDatabase (databaseV),
    execute,
    query_,
  )
import Housekeeping.Service.Hello.Handler
import RIO

selectMessageImpl :: ViewDatabase env => RIO env [Text]
selectMessageImpl =
  map fromOnly
    <$> invoke
      (databaseV . query_)
      "SELECT msg FROM message"

insertMessageImpl :: ViewDatabase env => Text -> RIO env ()
insertMessageImpl msg =
  void $
    invoke
      (databaseV . execute)
      "INSERT INTO message (msg) VALUES (?)"
      (Only msg)

helloRepositoryImpl :: ViewDatabase env => HelloRepository env
helloRepositoryImpl =
  HelloRepository
    { _insertMessage = insertMessageImpl,
      _selectMessage = selectMessageImpl
    }