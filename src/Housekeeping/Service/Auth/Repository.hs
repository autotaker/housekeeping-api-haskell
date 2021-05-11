{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}

module Housekeeping.Service.Auth.Repository where

import Control.Env.Hierarchical
import Housekeeping.DataSource
import Housekeeping.Service.Auth.Interface
import Housekeeping.Service.Auth.Model
import Housekeeping.Session (userId, userName)
import RIO

userRepositoryImpl :: Has1 Database env => UserRepository env
userRepositoryImpl =
  UserRepository
    { findUserByUserName = findUser,
      createUser = create
    }
  where
    create user = do
      r <- runIF $ \Database {..} ->
        query
          "INSERT INTO user (user_name) VALUES (?) RETURNING user_id"
          (Only $ user ^. userName)
      case r of
        [Only i] -> pure $ user & userId .~ i
        rows ->
          error $
            "query returns non-single row: user = "
              ++ show user
              ++ ", returning = "
              ++ show rows
    findUser username = do
      r <- runIF $ \Database {..} ->
        query
          "SELECT user_name, user_id FROM user WHERE user_name = ?"
          (Only username)
      case r of
        [user] -> pure $ Just user
        [] -> pure Nothing
        _ -> error $ "multiple users are found for the same user_name: " ++ show username

authRepositoryImpl :: (Has1 Database env) => AuthRepository env
authRepositoryImpl =
  AuthRepository
    { findPasswordAuthByUserName = findPassword,
      upsertPasswordAuth = upsertPassword
    }
  where
    findPasswordSql =
      "SELECT u.user_id, u.user_name, a.hashed_password"
        <> " FROM user u INNER JOIN auth_password a"
        <> " ON u.user_id = a.user_id AND u.user_name = ?"
    findPassword usernm = runIF $ \Database {..} -> do
      rows <- query findPasswordSql (Only usernm)
      case rows of
        [user :. Only passwd] -> pure $ Just $ PasswordAuth user passwd
        [] -> pure Nothing
        _ -> error $ "multiple passwords are found for the same username: " ++ show usernm
    upsertPasswordSql =
      "INSERT INTO auth_password (user_id, hashed_password)"
        <> " VALUES (?, ?)"
        <> " ON CONFLICT (user_id)"
        <> " DO UPDATE SET hashed_password = EXCLUDED.hashed_password,"
        <> " updated_at = CURRENT_TIMESTAMP"
    upsertPassword auth = do
      let userid = auth ^. passwordAuthUser . userId
          passwd = auth ^. passwordAuthPass
      void $ runIF $ \Database {..} -> execute upsertPasswordSql (userid, passwd)
