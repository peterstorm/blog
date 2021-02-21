{-# LANGUAGE OverloadedStrings #-}

module Views.About where

import qualified Data.Map      as Map
import           Miso          (View)
import           Miso.Html
import qualified Miso.String   as Miso

import           Model
import           Action
import           Views.Template


aboutView :: Model -> View Action
aboutView _ = template $ hero
  where hero =
          main_ [ id_ "content", class_ "white-background" ]
            [ div_ [ class_ "custom-grid col-12" ]
              [ div_ [ class_ "grid-section1", style_ $ Map.fromList [( Miso.pack "background", Miso.pack "#fff")] ] []
              , div_ [ class_ "grid-section2", style_ $ Map.fromList [( Miso.pack "background", Miso.pack "#fff")] ] []
              , div_ [ class_ "grid-section3", style_ $ Map.fromList [( Miso.pack "background", Miso.pack "#fff")] ] []
              ]
            ]
