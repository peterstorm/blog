{-# LANGUAGE TemplateHaskell, QuasiQuotes, FlexibleContexts, OverloadedStrings, ConstraintKinds #-}
module InMemory where

import qualified Data.Map as M
import Data.Set
import Control.Lens
import Control.Concurrent.STM
import Control.Monad.Reader
import Control.Monad.Except
import Control.Monad.Error.Lens
import Text.StringRandom
import Data.Text hiding (find, cons)
import Data.List

import Auth

data State = State
  { _stateAuths :: [(UserId, Auth)]
  , _stateUnverifiedEmails :: M.Map VerificationCode Email
  , _stateVerifiedEmails :: Set Email
  , _stateUserIdCounter :: Int
  , _stateNotifications :: M.Map Email VerificationCode
  , _stateSessions :: M.Map SessionId UserId
  } deriving (Show, Eq)

makeLenses ''State

type TVarState = TVar State

data StateInTVar = StateInTVar { _tVarState :: TVarState }

makeClassy ''StateInTVar

initialState :: State
initialState = State 
  { _stateAuths = []
  , _stateUnverifiedEmails = mempty
  , _stateVerifiedEmails = mempty
  , _stateUserIdCounter = 0
  , _stateNotifications = mempty
  , _stateSessions = mempty
  }


type InMemory r e m = (MonadReader r m, MonadError e m, MonadIO m, HasStateInTVar r)

addAuth :: (InMemory r e m, AsRegistrationError e) => Auth -> m VerificationCode
addAuth auth = do
  tvarState' <- view tVarState
  state <- liftIO $ readTVarIO tvarState'
  vcode <- liftIO $ stringRandomIO "[A-Za-z0-9]{16}"
  let email = auth ^. authEmail
      authEmails =  state ^. stateAuths <&> ( _authEmail . snd)
      isDuplicate = anyOf folded (email ==) authEmails
  when isDuplicate $ throwing_ _RegistrationErrorEmailTaken
  let newUserID = state ^. stateUserIdCounter + 1
  let newState = state
        & stateUserIdCounter .~ newUserID
        & stateAuths %~ cons (newUserID, auth)
        & stateUnverifiedEmails . at vcode ?~ email
  liftIO $ atomically $ writeTVar tvarState' newState
  return vcode

setEmailAsVerified :: (InMemory r e m, AsEmailVerificationError e) => VerificationCode -> m ()
setEmailAsVerified vcode = do
  tvarState' <- view tVarState
  state <- liftIO $ readTVarIO tvarState'
  let mayEmail = state ^. stateUnverifiedEmails . at vcode
  case mayEmail of
    Nothing -> throwing_ _EmailerificationErrorInvalidCode
    Just email -> do
      let newState = state
            & stateUnverifiedEmails . at vcode .~ Nothing
            & stateVerifiedEmails . at email ?~ ()
      liftIO $ atomically $ writeTVar tvarState' newState

findUserByAuth :: InMemory r e m => Auth -> m (Maybe (UserId, Bool))
findUserByAuth auth = do
  state <- view tVarState >>= liftIO . readTVarIO
  let maybeId = fmap fst . find ((auth ==) . snd) $ state ^. stateAuths
  case maybeId of
    Nothing  -> return Nothing
    Just uID -> do
      let isVerified = state ^. stateVerifiedEmails & elem (_authEmail auth)
      pure $ Just(uID, isVerified)
  

findEmailFromUserId :: InMemory r e m => UserId -> m (Maybe Email)
findEmailFromUserId uId = do
  state <- view tVarState >>= liftIO . readTVarIO
  let maybeAuth = find ((uId ==) . fst) $ state ^. stateAuths
  pure $ maybeAuth <&> (_authEmail . snd)


notifyEmailVerification :: InMemory r e m => Email -> VerificationCode -> m ()
notifyEmailVerification email vCode = do
  tvarState' <- view tVarState
  state <- liftIO $ readTVarIO tvarState'
  liftIO $ atomically $ writeTVar tvarState' (state & stateNotifications . at email ?~ vCode)

getNotificationsForEmail :: InMemory r e m => Email -> m (Maybe VerificationCode)
getNotificationsForEmail email = do
  tvarState' <- view tVarState
  liftIO (readTVarIO tvarState') <&> view (stateNotifications . at email)

newSession :: InMemory r e m => UserId -> m SessionId
newSession uId = do
  tvarState' <- view tVarState
  sId <- liftIO $ stringRandomIO "[A-Za-z0-9]{16}" <&> ((pack . show $ uId) <>)
  state <- liftIO $ readTVarIO tvarState'
  liftIO $ atomically $ writeTVar tvarState' (state & stateSessions . at sId ?~ uId)
  pure sId

findUserBySessionId :: InMemory r e m => SessionId -> m (Maybe UserId)
findUserBySessionId sId = do
  tVarState' <- view tVarState
  liftIO (readTVarIO tVarState') <&> view (stateSessions . at sId)