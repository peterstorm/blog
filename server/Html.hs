{-# LANGUAGE OverloadedStrings #-}
module Html where

import qualified Lucid      as L
import qualified Lucid.Base as L
-- | Represents the top level Html code. Its value represents the body of the
-- page.

newtype HtmlPage a = HtmlPage a
  deriving (Eq, Show)

instance L.ToHtml a => L.ToHtml (HtmlPage a) where
    toHtmlRaw = L.toHtml
    toHtml (HtmlPage x) = do
        L.doctype_
        L.html_ [ L.lang_ "en" ] $ do
          L.head_ $ do
            L.title_ "peterstorm" 
            L.meta_
              [ L.name_ "description"
              , L.content_ "i have no idea what i'm doing"
              ]
            L.meta_ [L.charset_ "utf-8"]
            L.meta_
              [ L.name_ "viewport"
              , L.content_ "width=device-width, initial-scale=1, maximum-scale=1"
              ]
            L.with (L.link_ mempty)
              [ L.rel_ "shortcut icon"
              , L.type_ "image/x-icon"
              , L.href_ "static/images/favicon.png"
              ]
            cssRefAll "static/css/bootstrap.grid.min.css" 
            cssRefAll "static/css/style.css"
            cssRef "https://fonts.googleapis.com/css?family=Merriweather:300,400%7CRubik:400,500,700"
          L.body_ (L.toHtml x)
          jsRefAsync "static/all.js"
          jsRefAsync "static/js/modernizr-custom.min.js"
          jsRefAsync "static/js/jquery.min.js"
          jsRefAsync "static/js/imagesloaded.pkgd.min.js"
          jsRefAsync "static/js/masonry.pkgd.min.js"
          jsRefAsync "static/js/functions.js"
            where
              jsRefAsync href =
                L.with (L.script_ mempty)
                  [ L.makeAttribute "src" href
                  , L.makeAttribute "async" mempty
                  , L.makeAttribute "defer" mempty
                  ]
              cssRefAll href =
                L.with (L.link_ mempty)
                  [ L.rel_ "stylesheet"
                  , L.href_ href
                  , L.type_ "text/css"
                  , L.media_ "all"
                  ]
              cssRef href =
                L.with (L.link_ mempty)
                  [ L.href_ href
                  , L.rel_ "stylesheet"
                  ]
