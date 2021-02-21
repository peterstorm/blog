{-# LANGUAGE OverloadedStrings #-}

module Views.Header where

import Miso (View)
import Miso.Html

import Action
import Utils
import Routes

header :: View Action
header =
  header_ [ id_ "top", class_ "navbar header" ]
    [ div_ [ class_ "container" ]
      [ div_ [ class_ "inner" ]
        [ div_ [ class_ "site-menu" ]
          [ a_ [ href_ "/", onPreventClick $ ChangeURI homeLink, class_ "logo" ] [ img_ [ src_ "static/images/logo-storm.png", width_ "144", height_ "33", alt_ "Storm" ] ] ]
          , input_ [ class_ "menu-btn", type_ "checkbox", id_ "menu-btn" ]
          , label_ [ class_ "menu-icon", for_ "menu-btn" ] [ span_ [ class_ "navicon" ] [] ]
          , ul_ [ class_ "menu" ]
            [ li_ [] [ a_ [ href_ "/", onPreventClick $ ChangeURI homeLink ] [ text "HOME" ] ]
            , li_ [] [ a_ [ href_ "/about", onPreventClick $ ChangeURI aboutLink ] [ text "ABOUT" ] ]
            , li_ [] [ a_ [ href_ "/about", onPreventClick $ ChangeURI aboutLink ] [ text "BLOG" ] ]
            , li_ [] [ a_ [ href_ "/contact", onPreventClick $ ChangeURI contactLink ] [ text "CONTACT" ] ]
            ]
        ]
        ]
      ]
