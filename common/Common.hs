{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE CPP                        #-}

module Common where

import Control.Lens
import Data.Proxy ( Proxy(..) )
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
  | AddOne
  | SubtractOne
  | ChangeURI !Network.URI
  | HandleURIChange !Network.URI
  deriving (Show, Eq)

-- Holds a servant route tree of `View action`
type ViewRoutes = Home :<|> Flipped :<|> Categories

-- Home route, contains two buttons and a field
type Home = View Action

-- Flipped route, same as Home, but with the buttons flipped
type Flipped = "flipped" :> View Action

type Categories = "categories" :> View Action

makeLenses ''Model

-- | Handlers
handlers = homeView
  :<|> homeView
  :<|> homeView

-- View function of the Home route
homeView :: Model -> View Action
homeView _ = template $ hero
  where hero = 
          section_ [ class_ "top-header bg-pattern pad-125" ]
            [ div_ [ class_ "container" ]
              [ div_ [ class_ "row" ] 
                [ div_ [ class_ "col-md-10 offset-md-1 text-left pad-left-90 pad-right-90 line-link has-animation animate-in" ]
                  [ h1_ [ class_ "title-one" ] [ span_ [] [ text "Hi" ], text " , I am Peter Storm, a developer based in Denmark" ] 
                  ]
              ]
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
  section_ [ class_ "header-wrapper style-two" ]
    [ div_ [ class_ "container" ]
      [ div_ [ class_ "row" ]
        [ div_ [ class_ "col-lg-2 col-md-12 text-left" ]
          [ a_ [ href_ "/", class_ "logo" ] [ img_ [src_ "static/images/logo-2.png", alt_ "logo" ] ]
          , a_ [ href_ "#", class_ "menu-click" ] [ span_ [] [], span_ [] [], span_ [] [] ]
          ]
          , div_ [ class_ "col-lg-8 col-md-12" ]
              [ nav_ [ id_ "main-menu", class_ "text-center" ]
                [ ul_ []
                  [ li_ [] [ a_ [ href_ "/"] [ text "Home" ] ] 
                  , li_ [] [ a_ [ href_ "/categories", onPreventClick $ ChangeURI categoriesLink ] [ text "Categories" ] ] 
                  , li_ [] [ a_ [ href_ "/flipped", onPreventClick $ ChangeURI categoriesLink ] [ text "About" ] ]
                  , li_ [] [ a_ [ href_ "/flipped", onPreventClick $ ChangeURI categoriesLink ] [ text "Contact" ] ]
                  ]
                ]
              ]
          , div_ [ class_ "col-lg-2 col-md-4 text-right" ]
              [ ul_ [ class_ "soical-icon-font list-inline pt-2 pb-0" ]
                [ li_ [ class_ "list-inline-item" ] [ a_ [ href_ "#" ] [ i_ [ class_ "fa fa-facebook" ] [] ] ]
                , li_ [ class_ "list-inline-item" ] [ a_ [ href_ "#" ] [ i_ [ class_ "fa fa-twitter" ] [] ] ]
                , li_ [ class_ "list-inline-item" ] [ a_ [ href_ "#" ] [ i_ [ class_ "fa fa-linkedin" ] [] ] ]
                , li_ [ class_ "list-inline-item" ] [ a_ [ href_ "#" ] [ i_ [ class_ "fa fa-heart" ] [] ] ]
                ]
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
categoriesLink :: Network.URI
categoriesLink =
#if MIN_VERSION_servant(0,10,0)
    Servant.linkURI $ Servant.safeLink (Proxy @ViewRoutes) (Proxy @Categories)
#else
    safeLink (Proxy @ViewRoutes) (Proxy @Categories)
#endif

