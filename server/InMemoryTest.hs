{-# LANGUAGE  DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module InMemoryTest where

import Control.Monad.Except
import Control.Monad.Reader 
import Control.Lens
import Control.Concurrent.STM
import qualified InMemory as IM
import Auth

data AppError = RegAppErr RegistrationError
              | LoginAppErr LoginError
              | EmailValAppErr EmailValidationErr
              | EmailVeriAppErr EmailVerificationError
              deriving stock (Show, Eq)

makeClassyPrisms ''AppError

instance AsRegistrationError AppError where
  _RegistrationError = _RegAppErr . _RegistrationError

instance AsEmailVerificationError AppError where
  _EmailVerificationError = _EmailVeriAppErr . _EmailVerificationError

instance AsLoginError AppError where
  _LoginError = _LoginAppErr . _LoginError

newtype App a = App
  { unApp :: ReaderT IM.StateInTVar (ExceptT AppError IO) a }
  deriving newtype (Applicative, Functor, Monad, MonadReader IM.StateInTVar, MonadIO, MonadError AppError)

instance AuthRepo App where
  addAuth = IM.addAuth
  setEmailAsVerified = IM.setEmailAsVerified
  findUserByAuth = IM.findUserByAuth
  findEmailFromUserId = IM.findEmailFromUserId

instance EmailVerificationNotif App where
  notifyEmailVerification = IM.notifyEmailVerification

instance SessionRepo App where
  newSession = IM.newSession
  findUserBySessionId = IM.findUserBySessionId

runApp :: IM.StateInTVar -> App a -> IO (Either AppError a)
runApp state app = runExceptT $ runReaderT (unApp app) state

app :: App ()
app = do
  let email = either undefined id $ mkEmail "pkshdk@gmail.com"
      passw = either undefined id $ mkPassword "123a1awfAWEFAW123"
      auth = Auth email passw
  register auth
  maybeVcode <- IM.getNotificationsForEmail email
  case maybeVcode of
    Nothing -> liftIO $ putStrLn "failed"
    Just vcode -> do verifyEmail vcode
                     session <- login auth
                     maybeUserId <- resolveSessionId session
                     case maybeUserId of
                       Nothing -> liftIO $ putStrLn "failed"
                       Just userId -> do
                         registeredEmail <- getUser userId
                         case registeredEmail of
                           Nothing -> liftIO $ putStrLn "failed"
                           Just email -> liftIO $ print (session, userId, email) 

main :: IO ()
main = do
  state <- newTVarIO IM.initialState
  result <- runApp (IM.StateInTVar state) app
  case result of
    Left _ -> putStrLn "failed"
    Right _ -> pure ()








