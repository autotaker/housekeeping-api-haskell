module Housekeeping.Service.Auth.Handler where

import Control.Env.Hierarchical
import Control.Monad.Trans.Maybe
import qualified Crypto.BCrypt as BCrypt
import Data.Coerce (coerce)
import Housekeeping.DataSource (HasTransactionManager, transactional)
import Housekeeping.Service.Auth.Interface
import Housekeeping.Service.Auth.Model
import RIO (MonadTrans (lift), RIO, guard, isNothing, view, (^.))
import Servant.Auth.Server (AuthResult (Authenticated, BadPassword, NoSuchUser))

authHandlerImpl ::
  ( Has1 UserRepository env,
    Has1 AuthRepository env,
    Has1 PasswordHasher env,
    HasTransactionManager env
  ) =>
  AuthHandler env
authHandlerImpl =
  AuthHandler
    { _signinHandler = signinHandlerImpl,
      _signupHandler = signupHandlerImpl
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
  mUser <- lift $ runIF (\repo -> view findUserByUserName repo usernm)
  guard $ isNothing mUser
  lift $
    transactional $ do
      user <- runIF $ \repo ->
        view createUser repo $ User {_userName = usernm, _userId = -1}
      hashedPasswd <- runIF $ \hasher -> view hashPassword hasher passwd
      let auth = PasswordAuth user hashedPasswd
      runIF $ \repo -> view upsertPasswordAuth repo auth
      pure user

signinHandlerImpl ::
  Has1 AuthRepository env =>
  UserName ->
  PlainPassword ->
  RIO env (AuthResult User)
signinHandlerImpl usernm passwd = do
  mAuth <- runIF (\repo -> view findPasswordAuthByUserName repo usernm)
  case mAuth of
    Nothing -> pure NoSuchUser
    Just auth
      | let hashedPasswd = auth ^. passwordAuthPass,
        BCrypt.validatePassword (coerce hashedPasswd) (coerce passwd) ->
        pure $ Authenticated $ auth ^. passwordAuthUser
      | otherwise -> pure BadPassword
