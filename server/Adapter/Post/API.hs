{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}
module Adapter.Post.API where

import           Servant

import           Data.Text   (Text)
import qualified Domain.Post as Domain

type PostApi =
  "api" :> "v1" :> "posts" :>
    (                           Get '[JSON] [Domain.Post]
    :<|> Capture "slug" Text :> Get '[JSON] Domain.Post
    )
