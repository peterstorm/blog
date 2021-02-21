module Action where

import qualified Network.URI as Network

data Action
  = NoOp
  | ChangeURI !Network.URI
  | HandleURIChange !Network.URI
  | InitMasonry
  deriving (Show, Eq)

