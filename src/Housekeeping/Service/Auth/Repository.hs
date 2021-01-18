module Housekeeping.Service.Auth.Repository where

import Control.Method
import Housekeeping.DataSource
import Housekeeping.Service.Auth.Interface
import Housekeeping.Service.Auth.Model
import RIO

userRepositoryImpl :: ViewDatabase env => UserRepository env
userRepositoryImpl =
  UserRepository
    { _findUserByUserName = findUser,
      _createUser = create
    }
  where
    create user = do
      r <-
        invoke
          (databaseV . query)
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
      r <-
        invoke
          (databaseV . query)
          "SELECT user_name, user_id FROM user WHERE user_name = ?"
          (Only username)
      case r of
        [user] -> pure $ Just user
        [] -> pure Nothing
        _ -> error $ "multiple users are found the same user_name: " ++ show username