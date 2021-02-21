{-# LANGUAGE TemplateHaskell   #-}
module Model where

import           Control.Lens
import qualified Network.URI as Network

data Model
   = Model
     { _uri          :: !Network.URI
     }
     deriving (Eq, Show)

makeLenses ''Model

