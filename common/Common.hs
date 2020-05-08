{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE CPP                        #-}

module Common where

import Control.Lens
import Data.Proxy ( Proxy(..) )
import qualified Data.Map as Map
import qualified Servant.API as Servant
import Servant.API ( (:<|>)(..), (:>) )
#if MIN_VERSION_servant(0,10,0)
import qualified Servant.Links as Servant
#endif
import qualified Miso
import Miso ( View )
import Miso.Html
import qualified Miso.String as Miso
import qualified Network.URI as Network


data Model
   = Model
     { _uri          :: !Network.URI
     }
     deriving (Eq, Show)


data Action
  = NoOp
  | ChangeURI !Network.URI
  | HandleURIChange !Network.URI
  deriving (Show, Eq)

-- Holds a servant route tree of `View action`
type ViewRoutes = Home :<|> About

type Home = View Action


type About = "about" :> View Action

makeLenses ''Model

-- | Handlers
handlers = homeView
  :<|> aboutView

-- View function of the Home route
homeView :: Model -> View Action
homeView _ = template $ hero
  where hero = 
          main_ [ id_ "content", class_ "white_background" ]
            [ div_ [ class_ "container" ] 
              [ div_ [ class_ "row" ]
                [ div_ [ class_ "project-listing col-12" ]
                  [ div_ [ class_ "grid clearfix", stringProp "data-masonry" "{ \"itemSelector\": \".grid-item\", \"columnWidth\": \".grid-sizer\", \"gutter\": \".gutter-sizer\", \"percentPosition\": true, \"transitionDuration\": \"0.3s\" } "  ]
                    [ div_ [ class_ "grid-item grid-item-wide project-thumb welcome-message" ]
                      [ div_ [ class_ "inner" ]
                        [ h1_ [] [ text "Hi! I'm Peter Storm", a_ [ href_ "/about", onPreventClick $ ChangeURI aboutLink ] [ text "STORM" ] ]
                        ]
                      ]
                    ]
                  ]
                ]
              ]
            ]

aboutView :: Model -> View Action
aboutView _ = template $ hero
  where hero =
          main_ [ id_ "content", class_ "white_background" ]
            [ div_ [ class_ "custom-grid col-12" ]
              [ div_ [ class_ "grid-section1", style_ $ Map.fromList [( Miso.pack "background", Miso.pack "#fff")] ] [] 
              , div_ [ class_ "grid-section2", style_ $ Map.fromList [( Miso.pack "background", Miso.pack "#fff")] ] []  
              , div_ [ class_ "grid-section3", style_ $ Map.fromList [( Miso.pack "background", Miso.pack "#fff")] ] [] 
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

