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

import           ClassyPrelude          hiding (bracket)
import           Control.Lens
import           Control.Monad.Except
import           Control.Monad.Reader   ()
import           Data.ByteString        ()
import           Data.Pool
import           Database.Beam
import           Database.Beam.Postgres
import           Database.Beam.Postgres (SqlError (..))

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

withConnection :: DbOperations r e m => (Connection -> IO a) -> m a
withConnection f = do
  pool <- view dbConnectionPool
  result <- liftIO $ try $ withResource pool $ \conn -> f conn
  case result of
    Left err -> throwError $ _DbErrorCode # err
    Right v -> pure v

makePoolTest :: IO (Pool Connection)
makePoolTest = do
  let initPool  = createPool openConn closeConn 2 5 10
      openConn  = connectPostgreSQL "dbname=blog"
      closeConn = close
  pool <- initPool
  pure pool
