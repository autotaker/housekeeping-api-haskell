{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Housekeeping.DataSource
  ( HasConnectionPool (..),
    HasTransactionManager (..),
    transactional,
    withConnection,
    defaultTransactionManager,
    TransactionManager (..),
    HasDatabase (..),
    ViewDatabase (..),
    Database (..),
    execute,
    execute_,
    query,
    query_,
    returning,
    Only (..),
    (:.) (..),
    databaseImpl,
    Transactional (..),
  )
where

import Control.Method
import Data.Functor.Contravariant (Contravariant (..))
import Data.Pool (Pool, putResource, takeResource)
import Database.PostgreSQL.Simple (Connection, FromRow, Only (..), Query, ToRow, (:.) (..))
import qualified Database.PostgreSQL.Simple as Sql
import Housekeeping.Cascade (mapEnvMethod)
import Lens.Micro.Platform (SimpleGetter, makeLenses)
import RIO (IORef, Int64, Lens', MonadIO (liftIO), RIO, Typeable, bracket, bracketOnError_, newIORef, readIORef, view, writeIORef)

class HasConnectionPool env where
  type IConnection env
  connectionPoolL :: Lens' env (Pool (IConnection env))

class Transactional conn where
  begin :: conn -> IO ()
  commit :: conn -> IO ()
  rollback :: conn -> IO ()

instance Transactional Connection where
  begin = Sql.begin
  commit = Sql.commit
  rollback = Sql.rollback

data Database env = Database
  { _query :: forall q r. (Typeable q, ToRow q, Typeable r, FromRow r) => Query -> q -> RIO env [r],
    _query_ :: forall r. (Typeable r, FromRow r) => Query -> RIO env [r],
    _execute :: forall q. (ToRow q, Typeable q) => Query -> q -> RIO env Int64,
    _execute_ :: Query -> RIO env Int64,
    _returning ::
      forall q r.
      (Typeable q, ToRow q, Typeable r, FromRow r) =>
      Query ->
      [q] ->
      RIO env [r]
  }

instance Contravariant Database where
  contramap f Database {..} =
    Database
      { _query = mapEnvMethod f _query,
        _query_ = mapEnvMethod f _query_,
        _execute = mapEnvMethod f _execute,
        _execute_ = mapEnvMethod f _execute_,
        _returning = mapEnvMethod f _returning
      }

makeLenses ''Database

class HasDatabase env where
  databaseL :: Lens' env (Database env)

class ViewDatabase env where
  databaseV :: SimpleGetter env (Database env)

newtype TransactionManager conn = TransactionManager
  {_currentTransaction :: IORef (Maybe conn)}

makeLenses ''TransactionManager

defaultTransactionManager :: IO (TransactionManager env)
defaultTransactionManager = TransactionManager <$> newIORef Nothing

class (HasConnectionPool env, Transactional (IConnection env)) => HasTransactionManager env where
  transactionManagerL :: Lens' env (TransactionManager (IConnection env))

withConnection :: (HasTransactionManager env) => ((IConnection env -> RIO env a) -> RIO env a)
withConnection action = do
  mConnRef <- view (transactionManagerL . currentTransaction)
  mConn <- readIORef mConnRef
  case mConn of
    Just conn -> action conn -- transactional connection
    Nothing -> do
      -- non-transactional connection
      pool <- view connectionPoolL
      bracket
        (liftIO $ takeResource pool)
        (\(conn, localPool) -> liftIO $ putResource localPool conn)
        (action . fst)

{-# INLINE transactional #-}
transactional ::
  ( Method method,
    Base method ~ RIO env,
    HasTransactionManager env,
    Transactional (IConnection env)
  ) =>
  method ->
  method
transactional method = curryMethod $ \args ->
  withConnection $ \conn ->
    bracketOnError_ (liftIO $ begin conn) (liftIO $ rollback conn) $ do
      mConnRef <- view (transactionManagerL . currentTransaction)
      writeIORef mConnRef (Just conn)
      a <- uncurryMethod method args
      liftIO $ commit conn
      pure a

databaseImpl :: (HasTransactionManager env, IConnection env ~ Connection) => Database env
databaseImpl =
  Database
    { _query = \sql q -> withConnection (\conn -> liftIO $ Sql.query conn sql q),
      _query_ = \sql -> withConnection (\conn -> liftIO $ Sql.query_ conn sql),
      _execute = \sql q -> withConnection (\conn -> liftIO $ Sql.execute conn sql q),
      _execute_ = \sql -> withConnection (\conn -> liftIO $ Sql.execute_ conn sql),
      _returning = \sql q -> withConnection (\conn -> liftIO $ Sql.returning conn sql q)
    }