{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}

module Adapter.Post.Types where

import           Data.Text     (Text)
import           Data.Time
import           Data.UUID
import           Database.Beam

data PostT f = Post
  { _postId        :: Columnar f UUID
  , _postSlug      :: Columnar f Text
  , _postTitle     :: Columnar f Text
  , _postBody      :: Columnar f Text
  , _postAuthor    :: Columnar f Text
  , _postCreatedAt :: Columnar f UTCTime
  , _postUpdatedAt :: Columnar f UTCTime
  } deriving stock (Generic)

type Post = PostT Identity
type PostId = PrimaryKey PostT Identity

deriving instance Show PostId
deriving instance Show Post

instance Beamable PostT
instance Beamable (PrimaryKey PostT)

instance Table PostT where
  data PrimaryKey PostT f = PostId (Columnar f UUID) deriving Generic
  primaryKey = PostId . _postId
