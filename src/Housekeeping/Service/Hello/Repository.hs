{-# LANGUAGE OverloadedStrings #-}

module Housekeeping.Service.Hello.Repository
  ( helloRepositoryImpl,
  )
where

import Housekeeping.DataSource
  ( HasTransactionManager (..),
    Only (Only, fromOnly),
    execute,
    query_,
    withConnection,
  )
import Housekeeping.Service.Hello.Handler
import RIO

selectMessageImpl :: HasTransactionManager env => RIO env [Text]
selectMessageImpl = withConnection $ \conn -> do
  liftIO $ map fromOnly <$> query_ conn "SELECT msg FROM message"

insertMessageImpl :: HasTransactionManager env => Text -> RIO env ()
insertMessageImpl msg = withConnection $ \conn -> do
  void $ liftIO $ execute conn "INSERT INTO message (msg) VALUES (?)" (Only msg)

helloRepositoryImpl :: HasTransactionManager env => HelloRepository env
helloRepositoryImpl =
  HelloRepository
    { _insertMessage = insertMessageImpl,
      _selectMessage = selectMessageImpl
    }