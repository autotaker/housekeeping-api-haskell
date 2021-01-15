{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Housekeeping.DataSource
  ( HasConnectionPool (..),
    HasTransactionManager (..),
    transactional,
    withConnection,
    defaultTransactionManager,
    TransactionManager (..),
    module Database.PostgreSQL.Simple,
  )
where

import Control.Method
import Data.Pool (Pool, putResource, takeResource)
import Database.PostgreSQL.Simple
import Lens.Micro.Platform (makeLenses, (?~))
import RIO (Lens', MonadIO (liftIO), MonadReader (local), RIO, bracket, bracketOnError_, view)

class HasConnectionPool env where
  connectionPoolL :: Lens' env (Pool Connection)

newtype TransactionManager = TransactionManager
  { _currentTransaction :: Maybe Connection
  }

makeLenses ''TransactionManager

defaultTransactionManager :: TransactionManager
defaultTransactionManager = TransactionManager Nothing

class HasConnectionPool env => HasTransactionManager env where
  transactionManagerL :: Lens' env TransactionManager

withConnection :: HasTransactionManager env => ((Connection -> RIO env a) -> RIO env a)
withConnection action = do
  mConn <- view (transactionManagerL . currentTransaction)
  case mConn of
    Just conn -> action conn
    Nothing -> do
      pool <- view connectionPoolL
      bracket
        (liftIO $ takeResource pool)
        (\(conn, localPool) -> liftIO $ putResource localPool conn)
        (action . fst)

{-# INLINE transactional #-}
transactional ::
  ( Method method,
    Base method ~ RIO env,
    HasTransactionManager env
  ) =>
  method ->
  method
transactional method = curryMethod $ \args ->
  withConnection $ \conn ->
    bracketOnError_ (liftIO $ begin conn) (liftIO $ rollback conn) $ do
      a <- local (transactionManagerL . currentTransaction ?~ conn) $ uncurryMethod method args
      liftIO $ commit conn
      pure a