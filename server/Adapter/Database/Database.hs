{-# LANGUAGE ConstraintKinds   #-}
{-# LANGUAGE KindSignatures    #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TemplateHaskell   #-}
module Adapter.Database.Database where

import           ClassyPrelude hiding (try, bracket)
import           Control.Lens
import           Control.Monad.Except
import Control.Monad.Catch hiding (try)
import           Control.Monad.Reader()
import           Data.ByteString()
import           Data.Pool
import           Data.Time
import           Database.Beam.Postgres

data DbConfig = DbConfig
  { _dbConnectionPool      :: Pool Connection
  , _dbConnectionString    :: ByteString
  , _dbStripeCount         :: Int
  , _dbMaxOpenConnPrStripe :: Int
  , _dbIdleConnTimeout     :: NominalDiffTime
  }

makeClassy ''DbConfig

data DbError = DbErrorCode SqlError | UnknownError
  deriving Show

makeClassyPrisms ''DbError

type DbOperations r e m = (HasDbConfig r, AsDbError e, MonadError e m, MonadReader r m, MonadIO m)

withConnection :: DbOperations r e m =>  (Connection -> IO a) -> m (Either e a)
withConnection f = do
  pool <- view dbConnectionPool
  try $ liftIO . withResource pool $ \conn -> f conn

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

try :: forall e (m :: * -> *) a. (MonadError e m, AsDbError e) => m a -> m (Either e a)
try a = catchError (Right `liftM` a) (pure . Left)
