{-# LANGUAGE TemplateHaskell, QuasiQuotes, FlexibleContexts, OverloadedStrings, ConstraintKinds #-}
module InMemory.Auth where

import qualified Data.Map as M
import Data.Set
import Control.Lens
import Control.Concurrent.STM
import Control.Monad.Reader
import Control.Monad.Except

import qualified Auth as A

data State = State
  { _stateAuths :: [(A.UserId, A.Auth)]
  , _stateUnverifiedEmails :: M.Map A.VerificationCode A.Email
  , _stateVerifiedEmails :: Set A.Email
  , _stateUserIdounter :: Int
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
findEmailFromUserId = undefined

notifyEmailVerification :: InMemory r e m => A.Email -> A.VerificationCode -> m ()
notifyEmailVerification = undefined

newSession :: InMemory r e m => A.UserId -> m A.SessionId
newSession = undefined

findUserBySessionId :: InMemory r e m => A.SessionId -> m (Maybe A.UserId)
findUserBySessionId sId = do
  tVarState' <- view tVarState
  liftIO $ readTVarIO tVarState' >>= (\s -> s ^. stateSessions ^.at sId & pure)
