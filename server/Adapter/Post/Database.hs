{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeSynonymInstances  #-}

module Adapter.Post.Database where


import           Data.Time
import           Data.UUID.V4
import           Database.Beam
import           Database.Beam.Postgres
import qualified Data.ByteString as BS
import Control.Monad.Reader (runReaderT, ReaderT)
import Control.Monad.Except (runExceptT, ExceptT)

import           Adapter.Database.Database
import           Adapter.Post.Types        as DB

createTestPosts :: IO [Post]
createTestPosts = do
  postId <- liftIO $ nextRandom
  postId' <- liftIO $ nextRandom
  time' <- liftIO $ getCurrentTime
  let testPosts = [ (DB.Post postId "test" "test" "test" "test" time' time')
                  , (DB.Post postId' "test1" "test1" "test1" "test1" time' time')]
  pure testPosts

insertPostsDebug :: DbOperations r e m => m (Either e ())
insertPostsDebug = do
  testPosts <- liftIO $ createTestPosts
  withConnection (\c ->
    runBeamPostgresDebug putStrLn c $ runInsert $
      insert (_blogPosts blogDb) $ insertValues testPosts)

runTest :: DbConfig -> ExceptT DbError (ReaderT DbConfig IO) a -> IO (Either DbError a)
runTest config = flip runReaderT config . runExceptT

runInit :: IO DbConfig
runInit = do
  pool <- makePoolTest
  pure $ DbConfig pool

main :: IO ()
main = do
  c <- runInit
  e <- runTest c $ insertPostsDebug
  case e of
    Left _ -> putStrLn "failed"
    Right _ -> pure ()
  



