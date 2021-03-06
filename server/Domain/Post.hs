{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
module Domain.Post where

import           Control.Lens hiding ((.=))
import           Data.Aeson
import           Data.Set
import           Data.Text
import           Data.Time
import           Data.UUID
import           GHC.Generics



data Post = Post
  { _postId        :: UUID
  , _postSlug      :: Text
  , _postTitle     :: Text
  , _postBody      :: Text
  , _postTagList   :: Set Text
  , _postCreatedAt :: UTCTime
  , _postUpdatedAt :: UTCTime
  , _postAuthor    :: Text
  } deriving (Show, Generic)

makeLenses ''Post

instance ToJSON Post
instance FromJSON Post

class Monad m => PostOperations m where
  getAllPosts :: m [Post]
  getPostBySlug :: Text -> m (Maybe Post)
  createPost :: Post -> m ()

--instance ToJSON PostId where
--  toJSON (PostId i) = toJSON i

--instance ToJSON Post where
--  toJSON (Post{..}) =
--    object [ "id"            .= _postId
--           , "slug"          .= _postSlug
--           , "postTitle"     .= _postTitle
--           , "postSubTitle"  .= _postSubTitle
--           , "postBody"      .= _postBody
--           , "postTagList"   .= _postTagList
--           , "postCreatedAt" .= _postCreatedAt
--           , "postUpdatedAt" .= _postUpdatedAt
--           , "postAuthor"    .= _postAuthor
--           ]
--
--instance FromJSON PostId where
--  parseJSON v = PostId <$> parseJSON v
--
--instance FromJSON Post where
--  parseJSON (Object v) =
--    Post <$> v .: "id"
--         <*> v .: "slug"
--         <*> v .: "postTitle"
--         <*> v .: "postSubTitle"
--         <*> v .: "postBody"
--         <*> v .: "postTagList"
--         <*> v .: "postCreatedAt"
--         <*> v .: "postUpdatedAt"
--         <*> v .: "postAuthor"
