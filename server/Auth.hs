{-# LANGUAGE TemplateHaskell, QuasiQuotes, FlexibleContexts, OverloadedStrings, DerivingStrategies #-}
module Auth (
  Auth(..),
  authEmail,
  Email,
  mkEmail,
  rawEmail,
  Password,
  mkPassword,
  rawPassword,
  UserId,
  VerificationCode,
  SessionId,
  AppError(..),
  AsAppError(..),
  AsRegistrationError(..),
  AsEmailVerificationError(..),
  AsLoginError(..),
  AuthRepo(..),
  EmailVerificationNotif(..),
  SessionRepo(..),
  register,
  verifyEmail,
  login,
  resolveSessionId,
  getUser
) where

import Control.Lens hiding (re)
import Control.Monad.Except
import Control.Monad.Error.Lens
import Data.Text
import Text.Regex.PCRE.Heavy
import Katip

import Validation

newtype Email = Email { emailRaw :: Text } deriving (Show, Eq, Ord)

rawEmail :: Email -> Text
rawEmail = emailRaw

mkEmail :: Text -> Either [Text] Email
mkEmail = validate Email
  [ regexMatches
    [re|^[A-Z0-9a-z._%+-]+@[A-Za-z0-9.-]+\.[A-Za-z]{2,64}$|]
    (pack "Not a valid email")
  ]

newtype Password = Password { passwordRaw :: Text } deriving (Show, Eq)

rawPassword :: Password -> Text
rawPassword = passwordRaw

mkPassword :: Text -> Either [Text] Password
mkPassword = validate Password
  [ lengthBetween 5 50 $ "Should be between 5 a ' ' ''State'State'Statend 50"
  , regexMatches [re|\d|] $ "Should contain number"
  , regexMatches [re|[A-Z]|] $ "Should contain uppercase letter"
  , regexMatches [re|[a-z]|] $ "Should contain lowercase letter"
  ]

data Auth = Auth
  { _authEmail :: Email
  , _authPassword :: Password
  } deriving (Show, Eq)

makeLenses ''Auth

type UserId = Int

type SessionId = Text

data LoginError = LoginErrorInvalidAuth
                | LoginErrorEmailNotVerified
                deriving (Show, Eq)

makeClassyPrisms ''LoginError

data RegistrationError = RegistrationErrorEmailTaken
                       | ToDo
  deriving (Show, Eq)

makeClassyPrisms ''RegistrationError

data EmailValidationError = EmailValidationErrorInvalidEmail
                        | Todo
                        deriving (Show, Eq)

makeClassyPrisms ''EmailValidationError

data EmailVerificationError = EmailerificationErrorInvalidCode
                            | Todo1
                            deriving (Show, Eq)

makeClassyPrisms ''EmailVerificationError

data AppError = RegAppErr RegistrationError
              | LoginAppErr LoginError
              | EmailValAppErr EmailValidationError
              | EmailVeriAppErr EmailVerificationError
              deriving stock (Show, Eq)

makeClassyPrisms ''AppError

instance AsRegistrationError AppError where
  _RegistrationError = _RegAppErr . _RegistrationError

instance AsEmailVerificationError AppError where
  _EmailVerificationError = _EmailVeriAppErr . _EmailVerificationError

instance AsLoginError AppError where
  _LoginError = _LoginAppErr . _LoginError

instance AsEmailValidationError AppError where
  _EmailValidationError = _EmailValAppErr . _EmailValidationError


type VerificationCode = Text

class Monad m => AuthRepo m where
  addAuth :: Auth -> m (UserId, VerificationCode)
  setEmailAsVerified :: VerificationCode -> m (UserId, Email)
  findUserByAuth :: Auth -> m (Maybe (UserId, Bool))
  findEmailFromUserId :: UserId -> m (Maybe Email)

class Monad m => EmailVerificationNotif m where
  notifyEmailVerification :: Email -> VerificationCode -> m ()

class Monad m => SessionRepo m where
  newSession :: UserId -> m SessionId
  findUserBySessionId :: SessionId -> m (Maybe UserId)

register :: (KatipContext m, MonadError e m, AuthRepo m, EmailVerificationNotif m, AsRegistrationError e) => Auth -> m ()
register auth = do
  (uId, vCode) <- addAuth auth
  let email = auth ^. authEmail
  notifyEmailVerification email vCode
  withUserIdContext uId $
    $(logTM) InfoS $ ls (rawEmail email) <> " is registered succesfully"

withUserIdContext :: (KatipContext m) => UserId -> m a -> m a
withUserIdContext uId = katipAddContext (sl "userId" uId)
  

verifyEmail :: (KatipContext m, MonadError e m, AuthRepo m, AsEmailVerificationError e) => VerificationCode -> m ()
verifyEmail vcode = do 
  (uId, email) <- setEmailAsVerified vcode
  withUserIdContext uId $
    $(logTM) InfoS $ ls (rawEmail email) <> " is verified succesfully"
  pure ()

login :: (KatipContext m, MonadError e m, AuthRepo m, SessionRepo m, AsLoginError e) => Auth -> m SessionId
login auth = do
  result <- findUserByAuth auth
  case result of
    Nothing          -> throwing_ _LoginErrorInvalidAuth
    Just (_, False)  -> throwing_ _LoginErrorEmailNotVerified
    Just (uId, True) -> withUserIdContext uId $ do
      sId <- newSession uId
      $(logTM) InfoS $ ls (rawEmail $ auth ^. authEmail) <> " logged in succesfully"
      return sId

resolveSessionId :: SessionRepo m => SessionId -> m (Maybe UserId)
resolveSessionId = findUserBySessionId

getUser :: (AuthRepo m) => UserId -> m (Maybe Email)
getUser = findEmailFromUserId

