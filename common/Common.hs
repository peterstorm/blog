{-# LANGUAGE CPP               #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE TypeOperators     #-}

module Common where

import           Control.Lens
import qualified Data.Map      as Map
import           Data.Proxy    (Proxy (..))
import           Servant.API   ((:<|>) (..), (:>))
import qualified Servant.API   as Servant
#if MIN_VERSION_servant(0,10,0)
import qualified Servant.Links as Servant
#endif
import           Miso          (View)
import qualified Miso
import           Miso.Html
import qualified Miso.String   as Miso
import qualified Network.URI   as Network


data Model
   = Model
     { _uri          :: !Network.URI
     }
     deriving (Eq, Show)

makeLenses ''Model

data Action
  = NoOp
  | ChangeURI !Network.URI
  | HandleURIChange !Network.URI
  | InitMasonry
  deriving (Show, Eq)

-- Holds a servant route tree of `View action`
type ViewRoutes = Home :<|> About :<|> Wedding :<|> Contact

type Home = View Action

type About = "about" :> View Action

type Wedding = "bryllup" :> View Action

type Contact = "contact" :> View Action

type Link = Miso.MisoString

type ImgSrc = Miso.MisoString

type Title = Miso.MisoString

type Description = Miso.MisoString

-- | Handlers
handlers :: (Model -> View Action) :<|> (Model -> View Action) :<|> (Model -> View Action) :<|> (Model -> View Action)
handlers = homeView :<|> aboutView :<|> weddingView :<|> contactView

-- View function of the Home route
homeView :: Model -> View Action
homeView _ = template $ hero
  where hero =
          main_ [ id_ "content", class_ "white-background" ]
            [ div_ [ class_ "container" ]
              [ div_ [ class_ "row" ]
                [ div_ [ class_ "project-listing col-12", onCreated InitMasonry ]
                  [ div_ [ class_ "grid clearfix", stringProp "data-masonry" ("{ \"itemSelector\": \".grid-item\""
                  <> ", \"columnWidth\": \".grid-sizer\", \"gutter\": \".gutter-sizer\", \"percentPosition\": true, \"transitionDuration\": \"0.3s\" } ")  ]
                    [ div_ [ class_ "grid-item grid-item-wide project-thumb welcome-message" ]
                      [ div_ [ class_ "inner" ]
                        [ h1_ [] [ text "Hi! I'm Peter Storm, and this is a test! "
                                 , a_ [ href_ "/about", onPreventClick $ ChangeURI aboutLink ] [ text "Go to ABOUT!" ] ] ]
                      ]
                    , gridItem "/about" aboutLink "static/images/sample-square.png" "BON" "Create intrigue around bla bla products"
                    , gridItem "/about" aboutLink "static/images/sample-square.png" "FrockHub" "Luxury fashion resource by Saxon Campbell"
                    , div_ [ class_ "grid-sizer" ] []
                    , div_ [ class_ "gutter-sizer" ] []
                    ]
                  ]
                ]
              ]
            ]

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

weddingView :: Model -> View Action
weddingView _ =
          main_ [ id_ "content", class_ "white-background" ]
            [ div_ [ class_ "container single" ]
              [ div_ [ class_ "row" ]
                [ div_ [ class_ "col-12" ]
                  [ div_ [ class_ "post-featured-image" ]
                    [ img_ [ src_ "https://scontent.fbma4-1.fna.fbcdn.net/v/t31.0-8/20449176_10155586457802888_7483912760165311604_o.jpg?_nc_cat=101&_nc_sid=8bfeb9&_nc_ohc=KBbfZJAKmZAAX_x1P9U&_nc_ht=scontent.fbma4-1.fna&oh=733d38215d6855d2a2a043d2beb055c9&oe=5F74C450", width_ "1180", height_ "680", alt_ "" ]
                    ]
                  ]
                , div_ [ class_ "col-12 col-md-10 offset-md-1 col-lg-8 offset-lg-2" ]
                  [ article_ [ class_ "post has-thumbnail" ]
                    [ div_ [ class_ "post-header" ]
                      [ h2_ [] [ text "Jóhanna og Peter's Bryllup!" ]
                      , p_ [ class_ "meta-info" ]
                        [ span_ [ class_ "post-author" ] [ text "af ", a_ [ href_ "mailto:johannatummasardottir@gmail.com" ] [ text "Jóhanna og Peter" ] ]
                        , span_ [ class_ "post-date" ] [ text "14. August 2020" ]
                        ]
                      ]
                    , div_ [ class_ "post-content" ]
                      [ p_ [] [ text "This is a test to see if it actually works, and it seems it does! Let's see how it looks with more text" ]
                        , p_ [] [ text "Let's add another paragraph, and more text to see how it formats" ]
                      ]
                      , div_ [ class_ "grey-box", style_ $ Map.fromList [ (Miso.pack "background", Miso.pack "url(static/images/sample-square.png) center bottom no-repeat")
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
                  , div_ [ class_ "col-md-6" ]
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
                  , div_ [ class_ "col-md-6" ]
                      [ div_ [ class_ "grey-box", style_ $ Map.fromList [ (Miso.pack "background", Miso.pack "url(static/images/sample-square.png) center bottom no-repeat")
                                                                      , (Miso.pack "background-size", Miso.pack "cover")
                                                                      ] ]
                      [ div_ [  id_ "address-selector1", class_ "overlay eq-height" ]
                        [ div_ [ id_ "address-NLDTEST", class_ "address-container active" ]
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

template :: View Action -> View Action
template content =
  div_ []
    [ header
    , content
    , footer
    ]

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
            , li_ [] [ a_ [ href_ "/about", onPreventClick $ ChangeURI aboutLink ] [ text "CONTACT" ] ]
            ]
        ]
        ]
      ]

footer :: View Action
footer = div_ [] []


page404View :: View Action
page404View  =
    text "Yo, 404, page unknown. Go to / or /flipped. Shoo!"

onPreventClick :: Action -> Attribute Action
onPreventClick action =
  onWithOptions defaultOptions { preventDefault = True }
    "click" Miso.emptyDecoder (\() -> action)

-- Network.URI that points to the home route
homeLink :: Network.URI
homeLink =
#if MIN_VERSION_servant(0,10,0)
    Servant.linkURI $ Servant.safeLink (Proxy @ViewRoutes) (Proxy @Home)
#else
    safeLink (Proxy @ViewRoutes) (Proxy @Home)
#endif

-- Network.URI that points to the home route
aboutLink :: Network.URI
aboutLink =
#if MIN_VERSION_servant(0,10,0)
    Servant.linkURI $ Servant.safeLink (Proxy @ViewRoutes) (Proxy @About)
#else
    safeLink (Proxy @ViewRoutes) (Proxy @About)
#endif

weddingLink :: Network.URI
weddingLink  =
#if MIN_VERSION_servant(0,10,0)
    Servant.linkURI $ Servant.safeLink (Proxy @ViewRoutes) (Proxy @Wedding)
#else
    safeLink (Proxy @ViewRoutes) (Proxy @Wedding)
#endif

contactLink ::  Network.URI
contactLink =
#if MIN_VERSION_servant(0,10,0)
    Servant.linkURI $ Servant.safeLink (Proxy @ViewRoutes) (Proxy @Contact)
#else
    safeLink (Proxy @ViewRoutes) (Proxy @Contact)
#endif




