{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
module Adapter.Database.Database where

import           ClassyPrelude          hiding (bracket, try)
import           Control.Lens
import           Control.Monad.Catch    hiding (try)
import           Control.Monad.Except
import           Control.Monad.Reader   ()
import           Data.ByteString        ()
import           Data.Pool
import           Data.Time
import           Database.Beam
import           Database.Beam.Postgres

import           Adapter.Post.Types     as Post

data DbConfig = DbConfig
  { _dbConnectionPool      :: Pool Connection
  }

makeClassy ''DbConfig

data DbError = DbErrorCode SqlError | UnknownError
  deriving stock Show

makeClassyPrisms ''DbError

data BlogDb f = BlogDb
  { _blogPosts :: f (TableEntity Post.PostT)
  } deriving stock (Generic)
    deriving anyclass (Database Postgres)

blogDb :: DatabaseSettings be BlogDb
blogDb = defaultDbSettings

type DbOperations r e m = (HasDbConfig r, AsDbError e, MonadError e m, MonadReader r m, MonadIO m)

data Environment = Development
                 | Test
                 | Production
                 deriving stock (Eq, Show, Read)

envPool :: Environment -> Int
envPool Test        = 1
envPool Development = 1
envPool Production  = 8

withConnection :: DbOperations r e m =>  (Connection -> IO a) -> m (Either e a)
withConnection f = do
  pool <- view dbConnectionPool
  try $ liftIO . withResource pool $ \conn -> f conn

makePoolTest :: IO (Pool Connection)
makePoolTest = do
  let initPool  = createPool openConn closeConn 2 5 10
      openConn  = connectPostgreSQL "dbname=blog"
      closeConn = close
  pool <- initPool
  pure pool

{-
   withPool :: (MonadIO m, MonadReader r m, HasDbConfig r) => (Pool Connection -> IO a) -> m a
withPool action = do
  cfg <- ask
  let initPool = createPool openConn closeConn
                      (cfg ^. dbStripeCount)
                      (cfg ^. dbIdleConnTimeout)
                      (cfg ^. dbMaxOpenConnPrStripe)
      cleanPool = destroyAllResources
      openConn  = connectPostgreSQL (cfg ^. dbConnectionString)
      closeConn = close
  liftIO $ bracket initPool cleanPool action
  -}

try :: forall e (m :: * -> *) a. (MonadError e m, AsDbError e) => m a -> m (Either e a)
try a = catchError (Right `liftM` a) (pure . Left)
