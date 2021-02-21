{-# LANGUAGE CPP               #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE TypeOperators     #-}

module Common where

import           Servant.API   ((:<|>) (..))
import           Miso          (View)
import           Miso.Html

import           Views.Wedding
import           Views.Home
import           Views.About
import           Views.Contact
import           Model
import           Action

-- | Handlers
handlers :: (Model -> View Action) :<|> (Model -> View Action) :<|> (Model -> View Action) :<|> (Model -> View Action)
handlers = homeView :<|> aboutView :<|> weddingView :<|> contactView


page404View :: View Action
page404View  =
    text "Yo, 404, page unknown. Go to / or /flipped. Shoo!"




