{-# LANGUAGE OverloadedStrings #-}
module Views.Footer where

import           Miso          (View)
import           Miso.Html

import           Action


footer :: View Action
footer = div_ [] []
