{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeOperators              #-}

module Main where

import qualified Common
import           Data.Proxy
import qualified Network.HTTP.Types                   as HTTP
import qualified Network.Wai                          as Wai
import qualified Network.Wai.Handler.Warp             as Wai
import qualified Network.Wai.Middleware.Gzip          as Wai
import qualified Network.Wai.Middleware.RequestLogger as Wai
import qualified Lucid      as L
import qualified Lucid.Base as L
import qualified Servant
import           Servant ( (:>), (:<|>)(..) )
import qualified System.IO                            as IO
import           Html
import qualified Miso
import Miso ( View )

main :: IO ()
main = do
    IO.hPutStrLn IO.stderr "Running on port 3003..."

    Wai.run 3003 $ Wai.logStdout $ compress app
  where
    compress :: Wai.Middleware
    compress = Wai.gzip Wai.def { Wai.gzipFiles = Wai.GzipCompress }

app :: Wai.Application
app =
    Servant.serve (Proxy @ServerAPI) (static :<|> serverHandlers :<|> Servant.Tagged page404)
          where 
            static = Servant.serveDirectoryFileServer "static"


serverHandlers :: ServerHandler :<|> ServerHandler :<|> ServerHandler
serverHandlers = homeServer :<|> aboutServer :<|> weddingServer
  where 
    send f u = pure $ HtmlPage $ f Common.Model { Common._uri = u }
    homeServer = send Common.homeView Common.homeLink 
    aboutServer = send Common.aboutView Common.aboutLink
    weddingServer = send Common.weddingView Common.weddingLink

type ServerHandler = Servant.Handler (HtmlPage (View Common.Action))

page404 :: Wai.Application
page404 _ respond = respond $ Wai.responseLBS
    HTTP.status404 [("Content-Type", "text/html")] $
    L.renderBS $ L.toHtml $ HtmlPage $ Common.page404View


-- Converts the ClientRoutes (which are a servant tree of routes leading to
-- some `View action`) to lead to `Get '[Html] (HtmlPage (View Common.Action))`
type ServerRoutes
   = Miso.ToServerRoutes Common.ViewRoutes HtmlPage Common.Action

-- The server serves static files besides the ServerRoutes, among which is the
-- javascript file of the client.
type ServerAPI =
       StaticAPI
  :<|> (ServerRoutes
  :<|> Servant.Raw) -- This will show the 404 page for any unknown route

type StaticAPI = "static" :> Servant.Raw

