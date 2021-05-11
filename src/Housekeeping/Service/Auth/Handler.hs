{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}

module Housekeeping.Service.Auth.Handler where

import Control.Env.Hierarchical
import Control.Monad.Trans.Maybe
import qualified Crypto.BCrypt as BCrypt
import Data.Coerce (coerce)
import Data.Maybe (fromJust)
import Housekeeping.DataSource
  ( HasTransactionManager,
    transactional,
  )
import Housekeeping.Service.Auth.Interface
import Housekeeping.Service.Auth.Model
import RIO (MonadIO (liftIO), MonadTrans (lift), RIO, guard, isNothing, (^.))
import Servant.Auth.Server (AuthResult (Authenticated, BadPassword, NoSuchUser))

passwordHasherImpl :: PasswordHasher env
passwordHasherImpl =
  PasswordHasher
    { hashPassword = \(PlainPassword plain) -> do
        HashedPassword . fromJust <$> liftIO (BCrypt.hashPasswordUsingPolicy BCrypt.fastBcryptHashingPolicy plain)
    }

authHandlerImpl ::
  ( Has1 UserRepository env,
    Has1 AuthRepository env,
    Has1 PasswordHasher env,
    HasTransactionManager env
  ) =>
  AuthHandler env
authHandlerImpl =
  AuthHandler
    { signinHandler = signinHandlerImpl,
      signupHandler = signupHandlerImpl
    }

signupHandlerImpl ::
  ( Has1 AuthRepository env,
    Has1 UserRepository env,
    Has1 PasswordHasher env,
    HasTransactionManager env
  ) =>
  UserName ->
  PlainPassword ->
  RIO env (Maybe User)
signupHandlerImpl usernm passwd = runMaybeT $ do
  mUser <- lift $ runIF (\UserRepository {..} -> findUserByUserName usernm)
  guard $ isNothing mUser
  lift $
    transactional $ do
      user <- runIF $ \UserRepository {..} ->
        createUser $ User {_userName = usernm, _userId = -1}
      hashedPasswd <- runIF $ \PasswordHasher {..} -> hashPassword passwd
      let auth = PasswordAuth user hashedPasswd
      runIF $ \AuthRepository {..} -> upsertPasswordAuth auth
      pure user

signinHandlerImpl ::
  Has1 AuthRepository env =>
  UserName ->
  PlainPassword ->
  RIO env (AuthResult User)
signinHandlerImpl usernm passwd = do
  mAuth <- runIF $ \AuthRepository {..} -> findPasswordAuthByUserName usernm
  case mAuth of
    Nothing -> pure NoSuchUser
    Just auth
      | let hashedPasswd = auth ^. passwordAuthPass,
        BCrypt.validatePassword (coerce hashedPasswd) (coerce passwd) ->
        pure $ Authenticated $ auth ^. passwordAuthUser
      | otherwise -> pure BadPassword
