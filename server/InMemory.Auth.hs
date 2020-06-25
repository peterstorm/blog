{-# LANGUAGE TemplateHaskell, QuasiQuotes, FlexibleContexts, OverloadedStrings, ConstraintKinds, PartialTypeSignatures #-}
module InMemory.Auth where

import qualified Data.Map as M
import Data.Set
import Control.Lens
import Control.Concurrent.STM
import Control.Monad.Reader
import Control.Monad.Except
import Text.StringRandom
import Data.Text

import qualified Auth as A

data State = State
  { _stateAuths :: [(A.UserId, A.Auth)]
  , _stateUnverifiedEmails :: M.Map A.VerificationCode A.Email
  , _stateVerifiedEmails :: Set A.Email
  , _stateUserIdCounter :: Int
  , _stateNotifications :: M.Map A.Email A.VerificationCode
  , _stateSessions :: M.Map A.SessionId A.UserId
  } deriving (Show, Eq)

makeLenses ''State

type TVarState = TVar State

data StateInTVar = StateInTVar { _tVarState :: TVarState }

makeClassy ''StateInTVar


type InMemory r e m = (MonadReader r m, MonadError e m, MonadIO m, HasStateInTVar r)

addAuth :: InMemory r e m => A.Auth -> m (Either e A.VerificationCode)
addAuth = undefined

setEmailAsVerified :: InMemory r e m => A.VerificationCode -> m (Either e ())
setEmailAsVerified = undefined

findUserByAuth :: InMemory r e m => A.Auth -> m (Maybe (A.UserId, Bool))
findUserByAuth = undefined

findEmailFromUserId :: InMemory r e m => A.UserId -> m (Maybe A.Email)
findEmailFromUserId uId = undefined


notifyEmailVerification :: InMemory r e m => A.Email -> A.VerificationCode -> m ()
notifyEmailVerification email vCode = do
  tvarState' <- view tVarState
  state <- liftIO $ readTVarIO tvarState'
  liftIO $ atomically $ writeTVar tvarState' (state & stateNotifications . at email ?~ vCode)

getNotificationsForEmail :: InMemory r e m => A.Email -> m (Maybe A.VerificationCode)
getNotificationsForEmail email = do
  tvarState' <- view tVarState
  liftIO (readTVarIO tvarState') <&> view (stateNotifications . at email)

newSession :: InMemory r e m => A.UserId -> m A.SessionId
newSession uId = do
  tvarState' <- view tVarState
  sId <- liftIO $ stringRandomIO "[A-Za-z0-9]{16}" <&> ((pack . show $ uId) <>)
  state <- liftIO $ readTVarIO tvarState'
  liftIO $ atomically $ writeTVar tvarState' (state & stateSessions . at sId ?~ uId)
  pure sId


findUserBySessionId :: InMemory r e m => A.SessionId -> m (Maybe A.UserId)
findUserBySessionId sId = do
  tVarState' <- view tVarState
  liftIO (readTVarIO tVarState') <&> view (stateSessions . at sId)
