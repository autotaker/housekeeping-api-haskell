{-# LANGUAGE OverloadedStrings #-}

module Housekeeping.Service.Hello.Repository
  ( HasDataSource (..),
    helloRepositoryImpl,
  )
where

import Housekeeping.DataSource
import Housekeeping.Service.Hello.Handler
import RIO

selectMessageImpl :: HasDataSource env => RIO env [Text]
selectMessageImpl = withConnection $ \conn -> do
  liftIO $ map fromOnly <$> query_ conn "SELECT msg FROM message"

insertMessageImpl :: HasDataSource env => Text -> RIO env ()
insertMessageImpl msg = withConnection $ \conn -> do
  void $ liftIO $ execute conn "INSERT INTO message (msg) VALUES (?)" (Only msg)

helloRepositoryImpl :: HasDataSource env => HelloRepository env
helloRepositoryImpl =
  HelloRepository
    { insertMessage = insertMessageImpl,
      selectMessage = selectMessageImpl
    }