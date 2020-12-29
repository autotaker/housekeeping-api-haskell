{-# LANGUAGE OverloadedStrings #-}

module Housekeeping.Service.Hello.Repository
  ( HasDataSource (..),
    selectMessage,
    insertMessage,
  )
where

import Data.Pool
import Database.PostgreSQL.Simple
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

selectMessage :: HasDataSource env => RIO env [Text]
selectMessage = withConnection $ \conn -> do
  liftIO $ map fromOnly <$> query_ conn "SELECT msg FROM message"

insertMessage :: HasDataSource env => Text -> RIO env ()
insertMessage msg = withConnection $ \conn -> do
  void $ liftIO $ execute conn "INSERT INTO message (msg) VALUES (?)" (Only msg)