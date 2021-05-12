{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Housekeeping.DataSource
  ( transactional,
    withConnection,
    defaultTransactionManager,
    TransactionManager (..),
    Database (..),
    Only (..),
    (:.) (..),
    databaseImpl,
    Transactional (..),
    IConnection,
    HasTransactionManager,
  )
where

import Control.Env.Hierarchical (Has, getL)
import Control.Method (Method (Base, curryMethod, uncurryMethod))
import Data.Pool (Pool, putResource, takeResource)
import Database.PostgreSQL.Simple (Connection, FromRow, Only (..), Query, ToRow, (:.) (..))
import qualified Database.PostgreSQL.Simple as Sql
import RIO (IORef, Int64, MonadIO (liftIO), RIO, Typeable, bracket, bracketOnError_, newIORef, readIORef, view, writeIORef)

type family IConnection env

class Transactional conn where
  begin :: conn -> IO ()
  commit :: conn -> IO ()
  rollback :: conn -> IO ()

instance Transactional Connection where
  begin = Sql.begin
  commit = Sql.commit
  rollback = Sql.rollback

data Database env = Database
  { query :: forall q r. (Typeable q, ToRow q, Typeable r, FromRow r) => Query -> q -> RIO env [r],
    query_ :: forall r. (Typeable r, FromRow r) => Query -> RIO env [r],
    execute :: forall q. (ToRow q, Typeable q) => Query -> q -> RIO env Int64,
    execute_ :: Query -> RIO env Int64,
    returning ::
      forall q r.
      (Typeable q, ToRow q, Typeable r, FromRow r) =>
      Query ->
      [q] ->
      RIO env [r]
  }

newtype TransactionManager conn = TransactionManager (IORef (Maybe conn))

type HasTransactionManager env =
  ( Has (TransactionManager (IConnection env)) env,
    Has (Pool (IConnection env)) env,
    Transactional (IConnection env)
  )

defaultTransactionManager :: IO (TransactionManager env)
defaultTransactionManager = TransactionManager <$> newIORef Nothing

withConnection :: forall conn env a. (Has (Pool conn) env, Has (TransactionManager conn) env) => ((conn -> RIO env a) -> RIO env a)
withConnection action = do
  TransactionManager mConnRef <- view getL
  mConn <- readIORef mConnRef
  case mConn of
    Just conn -> action conn -- transactional connection
    Nothing -> do
      -- non-transactional connection
      pool <- view getL
      bracket
        (liftIO $ takeResource pool)
        (\(conn, localPool) -> liftIO $ putResource localPool conn)
        (action . fst)

{-# INLINE transactional #-}
transactional ::
  forall env method.
  ( Method method,
    Base method ~ RIO env,
    HasTransactionManager env
  ) =>
  method ->
  method
transactional method = curryMethod $ \args ->
  withConnection @(IConnection env) $ \conn ->
    bracketOnError_ (liftIO $ begin conn) (liftIO $ rollback conn) $ do
      TransactionManager mConnRef <- view getL
      writeIORef mConnRef (Just conn)
      a <- uncurryMethod method args
      liftIO $ commit conn
      pure a

databaseImpl :: (Has (Pool Connection) env, Has (TransactionManager Connection) env, IConnection env ~ Connection) => Database env
databaseImpl =
  Database
    { query = \sql q -> withConnection (\conn -> liftIO $ Sql.query conn sql q),
      query_ = \sql -> withConnection (\conn -> liftIO $ Sql.query_ conn sql),
      execute = \sql q -> withConnection (\conn -> liftIO $ Sql.execute conn sql q),
      execute_ = \sql -> withConnection (\conn -> liftIO $ Sql.execute_ conn sql),
      returning = \sql q -> withConnection (\conn -> liftIO $ Sql.returning conn sql q)
    }