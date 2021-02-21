{-# LANGUAGE OverloadedStrings #-}

module Utils where

import Action
import qualified Miso as Miso
import Miso (onWithOptions, defaultOptions, preventDefault, Attribute)

onPreventClick :: Action -> Attribute Action
onPreventClick action =
  onWithOptions defaultOptions { preventDefault = True }
    "click" Miso.emptyDecoder (\() -> action)

