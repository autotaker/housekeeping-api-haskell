module Housekeeping.Service.Auth.Repository where

import Control.Method
import Housekeeping.DataSource
import Housekeeping.Service.Auth.Interface
import Housekeeping.Service.Auth.Model

userRepositoryImpl :: ViewDatabase env => UserRepository env
userRepositoryImpl =
  UserRepository
    { _findUserByUserName = findUser,
      _createUser = undefined
    }
  where
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