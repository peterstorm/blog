{-# LANGUAGE OverloadedStrings #-}

module Views.Contact where

import qualified Data.Map      as Map
import qualified Miso.String as Miso
import Miso (View)
import Miso.Html

import Action
import Model
import Views.Template

contactView :: Model -> View Action
contactView _ = template $ content
  where content =
          main_ [ id_ "content", class_ "white-background" ]
            [ div_ [ class_ "container" ]
              [ div_ [ class_ "row eq-height-container" ]
                [ div_ [ class_ "col-md-6" ]
                  [ div_ [ class_ "grey-box", style_ $ Map.fromList [ (Miso.pack "background", Miso.pack "url(static/images/sample-square.png) center bottom no-repeat")
                                                                    , (Miso.pack "background-size", Miso.pack "cover")
                                                                    ] ]
                    [ div_ [  id_ "address-selector", class_ "overlay eq-height" ]
                      [ div_ [ id_ "address-NLD", class_ "address-container active" ]
                        [ div_ [ class_ "address-details" ]
                          [ h1_ [] [ text "We're here!" ]
                          , ul_ [ class_ "ul-custom-bullet" ]
                            [ li_ [] [ i_ [ class_ "saulticon-map" ] [], text "Test test", br_ [], text "test 1234", br_ [], text "Denmark" ]
                            , li_ [] [ i_ [ class_ "saulticon-call" ] [], text "1231241234123123" ]
                            , li_ [] [ i_ [ class_ "saulticon-mail" ] [], text "test@test.com" ]
                            ]
                          ]
                        , div_ [ class_ "map" ]
                            [ iframe_ [ width_ "600", height_ "450", style_ $ Map.singleton (Miso.pack "border") (Miso.pack "0"), src_ "https://www.google.com/maps/embed/v1/place?key=AIzaSyBgsIWBq3RlVwJ3msi0277fxh4gZgWLY8c&q=52.3746634,4.8592912&zoom=15&maptype=roadmap" ] []
                            ]
                        ]
                      , a_ [ href_ "#", class_ "bottom-button" ]
                          [ span_ [ class_ "show-layer" ] [ text "Locate on map", i_ [ class_ "saulticon-arrow-up" ] [] ]
                          , span_ [ class_ "hide-layer" ] [ text "Close map", i_ [ class_ "saulticon-arrow-down" ] [] ]
                          ]
                      ]
                    ]
                  ]
                ]
              ]
            ]
