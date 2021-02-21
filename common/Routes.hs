{-# LANGUAGE CPP               #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE TypeOperators     #-}

module Routes where

import           Data.Proxy    (Proxy (..))
import           Servant.API   ((:<|>) (..), (:>))
import qualified Servant.API   as Servant
#if MIN_VERSION_servant(0,10,0)
import qualified Servant.Links as Servant
#endif
import           Miso          (View)
import qualified Network.URI   as Network

import           Action
import           Model

-- Holds a servant route tree of `View action`
type ViewRoutes = Home :<|> About :<|> Wedding :<|> Contact

type Home = View Action

type About = "about" :> View Action

type Wedding = "bryllup" :> View Action

type Contact = "contact" :> View Action


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
