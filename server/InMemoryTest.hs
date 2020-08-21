{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
module InMemoryTest where

import Control.Monad.Except
import Control.Monad.Reader
import Control.Concurrent.STM
import Katip
import System.IO (stdout)
import ClassyPrelude hiding (newTVarIO)

import qualified InMemory as IM
import Auth

newtype App a = App
  { unApp :: ReaderT IM.StateInTVar (ExceptT AppError (KatipContextT IO)) a }
  deriving newtype (Applicative, Functor, Monad, MonadReader IM.StateInTVar, MonadIO, MonadError AppError, KatipContext, Katip)

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

withKatip :: (LogEnv -> IO a) -> IO a
withKatip app  =
  bracket createLogEnv closeScribes app
    where
      createLogEnv = do
        logEnv <- initLogEnv "blog" "dev"
        stdoutScribe <- mkHandleScribe ColorIfTerminal stdout InfoS V2
        registerScribe "stdout" stdoutScribe defaultScribeSettings logEnv

logSomething :: (KatipContext m) => m ()
logSomething = do
  $(logTM) InfoS "Log in no namespace"
  katipAddNamespace "ns1" $
    $(logTM) InfoS "Log in ns1"
  katipAddNamespace "ns2" $ do
    $(logTM) WarningS "Log in ns2"
    katipAddNamespace "ns3" $
      katipAddContext (sl "userId" $ show "12") $ do
        $(logTM) InfoS "Log in ns2.ns3 with userId context"
        katipAddContext (sl "country" $ show "Denmark") $
          $(logTM) InfoS "Log in ns2.ns3 with userID and country context"

runKatip :: IO ()
runKatip = withKatip $ \le ->
  runKatipContextT le () mempty logSomething


runApp :: LogEnv -> IM.StateInTVar -> App a -> IO (Either AppError a)
runApp le state app = runKatipContextT le () mempty $ runExceptT $ runReaderT (unApp app) state

application :: App ()
application = do
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
                           Just emailReg -> liftIO $ print (session, userId, emailReg) 

main :: IO ()
main = withKatip $ \le -> do
  state <- newTVarIO IM.initialState
  result <- runApp le (IM.StateInTVar state) application 
  case result of
    Left _ -> putStrLn "failed"
    Right _ -> runKatip








