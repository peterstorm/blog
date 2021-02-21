{-# LANGUAGE OverloadedStrings #-}

module Views.Home where

import           Miso          (View)
import           Miso.Html
import qualified Miso.String   as Miso
import qualified Network.URI   as Network

import           Model
import           Action
import           Utils
import           Views.Template
import           Routes


homeView :: Model -> View Action
homeView _ = template $ hero
  where hero =
          main_ [ id_ "content", class_ "white-background" ]
            [ div_ [ class_ "container" ]
              [ div_ [ class_ "row" ]
                [ div_ [ class_ "project-listing col-12", onCreated InitMasonry ]
                  [ div_ [ class_ "grid clearfix", stringProp "data-masonry" ("{ \"itemselector\": \".grid-item\""
                  <> ", \"columnwidth\": \".grid-sizer\", \"gutter\": \".gutter-sizer\", \"percentposition\": true, \"transitionduration\": \"0.3s\" } ")  ]
                    [ div_ [ class_ "grid-item grid-item-wide project-thumb welcome-message" ]
                      [ div_ [ class_ "inner" ]
                        [ h1_ [] [ text "hi! i'm peter storm, and this is a test! "
                                 , a_ [ href_ "/about", onPreventClick $ ChangeURI aboutLink ] [ text "go to about!" ] ] ]
                      ]
                    , gridItem "/about" aboutLink "static/images/sample-square.png" "bon" "create intrigue around bla bla products"
                    , gridItem "/about" aboutLink "static/images/sample-square.png" "frockhub" "luxury fashion resource by saxon campbell"
                    , gridItemWide"/about" aboutLink "static/images/sample-square.png" "frockhub" "luxury fashion resource by saxon campbell"
                    , gridItem "/about" aboutLink "static/images/sample-square.png" "frockhub" "luxury fashion resource by saxon campbell"
                    , gridItem "/about" aboutLink "static/images/sample-square.png" "frockhub" "luxury fashion resource by saxon campbell"
                    , gridItemWide "/about" aboutLink "static/images/sample-square.png" "frockhub" "luxury fashion resource by saxon campbell"
                    , gridItem "/about" aboutLink "static/images/sample-square.png" "frockhub" "luxury fashion resource by saxon campbell"
                    , gridItem "/about" aboutLink "static/images/sample-square.png" "frockhub" "luxury fashion resource by saxon campbell"
                    , div_ [ class_ "grid-sizer" ] []
                    , div_ [ class_ "gutter-sizer" ] []
                    ]
                  ]
                ]
              ]
            ]

gridItem :: Link -> Network.URI -> ImgSrc -> Title -> Description -> View Action
gridItem link uri' src title desc =
  div_ [  class_ "grid-item project-thumb" ]
    [ a_ [ href_ link, onPreventClick $ ChangeURI uri' ]
      [ img_ [ src_ src, width_ "900", height_ "900", alt_ "" ]
      , span_ [ class_ "project-thumb-details" ]
        [ span_ [ class_ "title" ] [ text title ]
        , span_ [ class_ "description" ] [ text desc ]
        ]
      , i_ [ class_ "saulticon-arrow-forward" ] []
      ]
    ]

gridItemWide :: Link -> Network.URI -> ImgSrc -> Title -> Description -> View Action
gridItemWide link uri' src title desc =
  div_ [  class_ "grid-item grid-item-wide project-thumb" ]
    [ a_ [ href_ link, onPreventClick $ ChangeURI uri' ]
      [ img_ [ src_ src, width_ "900", height_ "900", alt_ "" ]
      , span_ [ class_ "project-thumb-details" ]
        [ span_ [ class_ "title" ] [ text title ]
        , span_ [ class_ "description" ] [ text desc ]
        ]
      , i_ [ class_ "saulticon-arrow-forward" ] []
      ]
    ]

type Link = Miso.MisoString

type ImgSrc = Miso.MisoString

type Title = Miso.MisoString

type Description = Miso.MisoString

