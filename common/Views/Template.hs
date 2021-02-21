{-# LANGUAGE OverloadedStrings #-}

module Views.Template where

import           Miso          (View)
import           Miso.Html

import           Action
import           Views.Header
import           Views.Footer


template :: View Action -> View Action
template content =
  div_ []
    [ header
    , content
    , footer
    ]

