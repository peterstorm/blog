{-# LANGUAGE TemplateHaskell, QuasiQuotes, FlexibleContexts, OverloadedStrings #-}
module Auth where

import Control.Lens hiding (re)
import Control.Monad.Except
import Control.Monad.Error.Lens
import Data.Text
import Text.Regex.PCRE.Heavy


import Validation

newtype Email = Email { emailRaw :: Text } deriving (Show, Eq)

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
  [ lengthBetween 5 50 $ "Should be between 5 and 50"
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

data EmailValidationErr = EmailValidationErrorInvalidEmail
                        | Todo
                          deriving (Show, Eq)

makeClassyPrisms ''EmailValidationErr

data EmailVerificationError = EmailerificationErrorInvalidCode
                            | Todo1
  deriving (Show, Eq)

makeClassyPrisms ''EmailVerificationError

type VerificationCode = Text

class Monad m => AuthRepo m where
  addAuth :: Auth -> m (Either e VerificationCode)
  setEmailAsVerified :: VerificationCode -> m (Either e ())
  findUserByAuth :: Auth -> m (Maybe (UserId, Bool))
  findEmailFromUserId :: UserId -> m (Maybe Email)

class Monad m => EmailVerificationNotif m where
  notifyEmailVerification :: Email -> VerificationCode -> m ()

class Monad m => SessionRepo m where
  newSession :: UserId -> m SessionId
  findUserBySessionId :: SessionId -> m (Maybe UserId)

register :: (MonadError e m, AuthRepo m, EmailVerificationNotif m, AsRegistrationError e) => Auth -> m ()
register auth = do
  vCode <- addAuth auth
  let email = auth ^. authEmail
  case vCode of
    Left _ -> throwing_ _RegistrationErrorEmailTaken
    Right x -> notifyEmailVerification email x


verifyEmail :: (MonadError e m, AsEmailVerificationError e, AuthRepo m) => VerificationCode -> m (Either e ())
verifyEmail = setEmailAsVerified

login :: (MonadError e m, AuthRepo m, SessionRepo m, AsLoginError e) => Auth -> m SessionId
login auth = do
  result <- findUserByAuth auth
  case result of
    Nothing          -> throwing_ _LoginErrorInvalidAuth
    Just (_, False)  -> throwing_ _LoginErrorEmailNotVerified
    Just (uId, True) -> newSession uId

resolveSessionId :: SessionRepo m => SessionId -> m (Maybe UserId)
resolveSessionId = findUserBySessionId

getUser :: (AuthRepo m) => UserId -> m (Maybe Email)
getUser = findEmailFromUserId

instance AuthRepo m => AuthRepo (ExceptT e m) where
  addAuth = lift . addAuth
  setEmailAsVerified = undefined
  findUserByAuth = undefined
  findEmailFromUserId = undefined

instance EmailVerificationNotif m => EmailVerificationNotif (ExceptT e m) where
  notifyEmailVerification email code = lift $ notifyEmailVerification email code

-- for testing
errorString :: RegistrationError -> String
errorString RegistrationErrorEmailTaken = "failed, email taken"

instance AuthRepo IO where
  addAuth (Auth email pass) = do
    putStrLn $ "adding auth: " <> (unpack $ rawEmail email)
    pure $ Right "fake verification code"
  setEmailAsVerified = undefined
  findUserByAuth = undefined
  findEmailFromUserId = undefined

instance EmailVerificationNotif IO where
 notifyEmailVerification email vcode =
   putStrLn $ "Notify " <> (unpack $ rawEmail email) <> " - " <> (unpack vcode)

main :: IO ()
main = do
  let Right email = mkEmail (pack "test@oister.dk")
  let Right password = mkPassword (pack "123123AWEFAWEFAWawefawf")
  let auth = Auth email password
  result <- runExceptT $ register auth
  case result of
    Left x  -> putStrLn $ errorString x
    Right _ -> pure ()

