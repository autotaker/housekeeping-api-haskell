module Housekeeping.Service.Auth.Handler where

import Control.Method (invoke)
import Control.Monad.Trans.Maybe
import qualified Crypto.BCrypt as BCrypt
import Data.Coerce (coerce)
import Housekeeping.DataSource (HasTransactionManager, transactional)
import Housekeeping.Service.Auth.Interface
import Housekeeping.Service.Auth.Model
import RIO (MonadTrans (lift), RIO, guard, isNothing, (^.))
import Servant.Auth.Server (AuthResult (Authenticated, BadPassword, NoSuchUser))

authHandlerImpl ::
  ( ViewUserRepository env,
    ViewAuthRepository env,
    ViewPasswordHasher env,
    HasTransactionManager env
  ) =>
  AuthHandler env
authHandlerImpl =
  AuthHandler
    { _signinHandler = signinHandlerImpl,
      _signupHandler = signupHandlerImpl
    }

signupHandlerImpl ::
  ( ViewAuthRepository env,
    ViewUserRepository env,
    ViewPasswordHasher env,
    HasTransactionManager env
  ) =>
  UserName ->
  PlainPassword ->
  RIO env (Maybe User)
signupHandlerImpl = transactional $ \usernm passwd -> runMaybeT $ do
  mUser <- lift $ invoke (userRepositoryV . findUserByUserName) usernm
  guard $ isNothing mUser
  user <- lift $ invoke (userRepositoryV . createUser) $ User {_userName = usernm, _userId = -1}
  hashedPasswd <- lift $ invoke (passwordHasherV . hashPassword) passwd
  let auth = PasswordAuth user hashedPasswd
  lift $ invoke (authRepositoryV . upsertPasswordAuth) auth
  pure user

signinHandlerImpl ::
  ViewAuthRepository env =>
  UserName ->
  PlainPassword ->
  RIO env (AuthResult User)
signinHandlerImpl usernm passwd = do
  mAuth <- invoke (authRepositoryV . findPasswordAuthByUserName) usernm
  case mAuth of
    Nothing -> pure NoSuchUser
    Just auth
      | let hashedPasswd = auth ^. passwordAuthPass,
        BCrypt.validatePassword (coerce hashedPasswd) (coerce passwd) ->
        pure $ Authenticated $ auth ^. passwordAuthUser
      | otherwise -> pure BadPassword
