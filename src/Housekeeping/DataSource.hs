module Housekeeping.DataSource (HasDataSource (..), withConnection, module Database.PostgreSQL.Simple) where

import Data.Pool (Pool, putResource, takeResource)
import Database.PostgreSQL.Simple
import RIO (Lens', MonadIO (liftIO), RIO, bracket, view)

class HasDataSource env where
  dataSourceL :: Lens' env (Pool Connection)

withConnection :: HasDataSource env => (Connection -> RIO env a) -> RIO env a
withConnection action = do
  pool <- view dataSourceL
  bracket
    (liftIO $ takeResource pool)
    (\(conn, localPool) -> liftIO $ putResource localPool conn)
    (action . fst)