{-# LANGUAGE OverloadedStrings #-}

module Housekeeping.Service.Hello.Repository
  ( HasDataSource (..),
    helloRepository,
  )
where

import Data.Pool
import Database.PostgreSQL.Simple
import Housekeeping.Service.Hello.Handler
import RIO

class HasDataSource env where
  dataSourceL :: Lens' env (Pool Connection)

withConnection :: HasDataSource env => (Connection -> RIO env a) -> RIO env a
withConnection action = do
  pool <- view dataSourceL
  bracket
    (liftIO $ takeResource pool)
    (\(conn, localPool) -> liftIO $ putResource localPool conn)
    (action . fst)

selectMessageImpl :: HasDataSource env => RIO env [Text]
selectMessageImpl = withConnection $ \conn -> do
  liftIO $ map fromOnly <$> query_ conn "SELECT msg FROM message"

insertMessageImpl :: HasDataSource env => Text -> RIO env ()
insertMessageImpl msg = withConnection $ \conn -> do
  void $ liftIO $ execute conn "INSERT INTO message (msg) VALUES (?)" (Only msg)

helloRepository :: HasDataSource env => HelloRepository env
helloRepository =
  HelloRepository
    { insertMessage = insertMessageImpl,
      selectMessage = selectMessageImpl
    }