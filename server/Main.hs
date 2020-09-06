{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE TypeOperators     #-}

module Main where

import           Data.Proxy
import qualified Lucid                                as L
import           Miso                                 (View)
import qualified Miso
import qualified Network.HTTP.Types                   as HTTP
import qualified Network.Wai                          as Wai
import qualified Network.Wai.Handler.Warp             as Wai
import qualified Network.Wai.Middleware.Gzip          as Wai
import qualified Network.Wai.Middleware.RequestLogger as Wai
import           Servant                              ((:<|>) (..), (:>))
import qualified Servant
import qualified System.IO                            as IO

import qualified Common
import qualified Domain.Post as Domain
import           Html
import           Http.Api.Post
import Data.Text (Text)

main :: IO ()
main = do
    IO.hPutStrLn IO.stderr "Running on port 3003..."

    Wai.run 3003 $ Wai.logStdout $ compress app
  where
    compress :: Wai.Middleware
    compress = Wai.gzip Wai.def { Wai.gzipFiles = Wai.GzipCompress }

app :: Wai.Application
app =
    Servant.serve (Proxy @ServerAPI) (static :<|> serverHandlers :<|> Servant.Tagged page404 :<|> postServer)
          where
            static = Servant.serveDirectoryFileServer "static"

testHandlerGetPost :: Text -> Servant.Handler Domain.Post 
testHandlerGetPost = undefined

testHandlerGetPosts :: Servant.Handler [Domain.Post]
testHandlerGetPosts = undefined

postServer :: Servant.Server PostApi
postServer = testHandlerGetPosts :<|> testHandlerGetPost

serverHandlers :: ServerHandler :<|> ServerHandler :<|> ServerHandler :<|> ServerHandler
serverHandlers = homeServer :<|> aboutServer :<|> weddingServer :<|> contactServer
  where
    send f u = pure $ HtmlPage $ f Common.Model { Common._uri = u }
    homeServer = send Common.homeView Common.homeLink
    aboutServer = send Common.aboutView Common.aboutLink
    weddingServer = send Common.weddingView Common.weddingLink
    contactServer = send Common.contactView Common.contactLink

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
  :<|> Servant.Raw -- This will show the 404 page for any unknown routew
  :<|> PostApi) 

type StaticAPI = "static" :> Servant.Raw

